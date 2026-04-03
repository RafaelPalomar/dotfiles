#!/usr/bin/env python3
"""Module to connect ARM to MusicBrainz API, with GNUDB fallback."""

import logging
import re
import urllib.request
import urllib.parse
import musicbrainzngs as mb
import werkzeug
from discid import read, Disc

import arm.config.config as cfg
from arm.ripper import utils as u


werkzeug.cached_property = werkzeug.utils.cached_property

GNUDB_SERVER = "https://gnudb.gnudb.org"
GNUDB_HELLO = "arm+arm@localhost+arm+1.0"


def main(disc):
    """
    Depending on the configuration musicbrainz is used
    to identify the music disc. The label of the disc is returned
    or "".
    """
    discid = get_disc_id(disc)
    if cfg.arm_config['GET_AUDIO_TITLE'] == 'musicbrainz':
        return music_brainz(discid, disc)
    return ""


def get_disc_id(disc):
    """
    Calculates the identifier of the disc

    return:
    identification object from discid package
    """
    return read(disc.devpath)


def music_brainz(discid: str, job) -> str:
    """
    Query MusicBrainz for metadata about an audio disc and update the job with release info.

    Falls back to GNUDB (freedb-compatible) when MusicBrainz does not have the disc.
    """

    disc_info = get_disc_info(job, discid)
    if disc_info == "":
        logging.error("ARM has encountered an error and stopping")
        return ""

    artist_title = check_musicbrainz_data(job, disc_info)
    if artist_title == "":
        logging.error("ARM has encountered an error and stopping")
        return ""

    return artist_title


def get_disc_info(job, discid) -> dict | str:
    """
    Retrieve CD release information from MusicBrainz using the disc ID.
    If MusicBrainz does not have the disc (WebServiceError), fall back to GNUDB.

    Returns a dict compatible with check_musicbrainz_data() or "" on total failure.
    """

    # Tell musicbrainz what your app is, and how to contact you
    mb.set_useragent(app="arm", version=str(job.arm_version), contact="https://github.com/automatic-ripping-machine")

    try:
        disc_info = mb.get_releases_by_discid(discid, includes=['artist-credits', 'recordings'])
        logging.debug(f"discid: [{discid}]")
        return disc_info

    except mb.WebServiceError as exc:
        logging.warning(f"MusicBrainz lookup failed ({exc}); trying GNUDB fallback")

    # --- GNUDB fallback ---
    try:
        gnudb_result = _gnudb_lookup(discid)
        if gnudb_result:
            logging.info("GNUDB fallback succeeded")
            return gnudb_result
        logging.error("GNUDB fallback returned no data")
    except Exception as exc:  # pylint: disable=broad-except
        logging.error(f"GNUDB fallback error: {exc}")

    u.database_updater(False, job)
    return ""


def _gnudb_lookup(discid) -> dict | None:
    """
    Query GNUDB for the disc using the CDDB protocol over HTTP.

    Returns a synthetic disc_info dict structured like the MusicBrainz 'cdstub'
    response so that check_musicbrainz_data() can handle it without modification.
    Returns None if the disc is not found.
    """
    freedb_id = discid.freedb_id
    tracks = discid.tracks          # list of Track objects with .offset (sectors)
    track_count = len(tracks)

    # Build the CDDB query string:
    # cddb query <discid> <ntrks> <off1> <off2> … <offlast> <nsecs>
    # offsets are in sectors (75 sectors/sec); total seconds = lead-out / 75
    offsets = [str(t.offset) for t in tracks]
    total_secs = int(discid.sectors / 75)
    query_str = f"{freedb_id} {track_count} {' '.join(offsets)} {total_secs}"

    # Step 1: query — find the disc category and exact disc ID
    query_url = (
        f"{GNUDB_SERVER}/~cddb/cddb.cgi"
        f"?cmd=cddb+query+{urllib.parse.quote(query_str)}"
        f"&hello={urllib.parse.quote(GNUDB_HELLO)}"
        "&proto=6"
    )
    logging.debug(f"GNUDB query URL: {query_url}")

    with urllib.request.urlopen(query_url, timeout=10) as resp:
        query_response = resp.read().decode("utf-8", errors="replace")

    logging.debug(f"GNUDB query response: {query_response!r}")

    # Parse query response: first line is status code
    lines = query_response.splitlines()
    if not lines:
        return None

    status_line = lines[0]
    # 200 = exact match, 211 = inexact matches (take first), 210 = multiple exact
    if status_line.startswith("200 "):
        # "200 <category> <discid> <title>"
        parts = status_line[4:].split(None, 2)
        if len(parts) < 3:
            return None
        category, exact_id = parts[0], parts[1]
    elif status_line.startswith("210 ") or status_line.startswith("211 "):
        # Multiple matches follow, one per line, terminated by "."
        # Take the first one
        match_line = lines[1] if len(lines) > 1 else ""
        if not match_line or match_line == ".":
            return None
        parts = match_line.split(None, 2)
        if len(parts) < 2:
            return None
        category, exact_id = parts[0], parts[1]
    else:
        # 202 = no match, 403/500 = errors
        logging.info(f"GNUDB query: no match (status: {status_line!r})")
        return None

    # Step 2: read — fetch the full disc record
    read_url = (
        f"{GNUDB_SERVER}/~cddb/cddb.cgi"
        f"?cmd=cddb+read+{urllib.parse.quote(category)}+{urllib.parse.quote(exact_id)}"
        f"&hello={urllib.parse.quote(GNUDB_HELLO)}"
        "&proto=6"
    )
    logging.debug(f"GNUDB read URL: {read_url}")

    with urllib.request.urlopen(read_url, timeout=10) as resp:
        read_response = resp.read().decode("utf-8", errors="replace")

    logging.debug(f"GNUDB read response (first 500 chars): {read_response[:500]!r}")

    return _parse_gnudb_response(read_response, exact_id, track_count)


def _parse_gnudb_response(response: str, disc_id: str, track_count: int) -> dict | None:
    """
    Parse a GNUDB/CDDB read response into a synthetic 'cdstub'-style dict.

    The dict has the same structure expected by check_musicbrainz_data():
        { 'cdstub': {
              'id': <str>,
              'title': <str>,
              'artist': <str>,
              'track-count': <int>,
              'track-list': [ {'title': <str>, 'length': <str>}, ... ]
          } }
    """
    lines = response.splitlines()
    if not lines or not lines[0].startswith("210 "):
        logging.warning(f"GNUDB read: unexpected status line {lines[0]!r if lines else '(empty)'}")
        return None

    fields = {}
    for line in lines[1:]:
        if line == ".":
            break
        if line.startswith("#"):
            continue
        if "=" in line:
            key, _, val = line.partition("=")
            key = key.strip()
            val = val.strip()
            if key in fields:
                fields[key] += val   # continuation lines for long values
            else:
                fields[key] = val

    dtitle = fields.get("DTITLE", "Unknown / Unknown")
    # DTITLE is typically "Artist / Album"
    if " / " in dtitle:
        artist, _, title = dtitle.partition(" / ")
    else:
        artist = dtitle
        title = dtitle

    artist = artist.strip()
    title = title.strip()
    dyear = fields.get("DYEAR", "").strip()

    # Build track list from TTITLEn fields
    track_list = []
    for i in range(track_count):
        track_title = fields.get(f"TTITLE{i}", f"Track {i + 1}").strip()
        # GNUDB doesn't provide track lengths in the record; use 0
        track_list.append({"title": track_title, "length": "0"})

    result = {
        "cdstub": {
            "id": disc_id,
            "title": title,
            "artist": artist,
            "track-count": track_count,
            "track-list": track_list,
        }
    }

    # Inject year into a synthetic 'disc' wrapper so check_date() can pick it up
    # if we ever restructure; for now cdstub path ignores year, so store in result
    # for logging purposes only.
    if dyear:
        result["cdstub"]["_year"] = dyear
        logging.info(f"GNUDB year: {dyear}")

    logging.info(f"GNUDB parsed: artist={artist!r} title={title!r} tracks={track_count}")
    return result


def check_musicbrainz_data(job, disc_info: dict) -> str:
    """
    Process MusicBrainz metadata for a disc or CD stub and update the job database.

    Handles both 'disc' (full MusicBrainz release) and 'cdstub' (limited data,
    including GNUDB fallback results).
    """

    music_data = ""

    if 'disc' not in disc_info and 'cdstub' not in disc_info:
        logging.error("No release information reported by MusicBrainz")
        return music_data

    if 'disc' in disc_info:
        logging.info("Processing as a disc")
        release_list = disc_info['disc'].get('release-list', [])
        logging.debug(f"Number of releases: {len(release_list)}")

        if len(release_list) > 0:
            for i in range(len(release_list)):
                logging.debug(f"Checking release: [{i}] if CD")
                medium_list = release_list[i].get('medium-list', [])
                if medium_list and medium_list[0].get('format') == "CD":
                    logging.info(f"Release [{i}] is a CD, tracking on...")
                    logging.debug("-" * 50)
                    process_tracks(job, medium_list[0].get('track-list'))
                    logging.debug("-" * 50)

                    release = disc_info['disc']['release-list'][i]
                    new_year = check_date(release)
                    title = str(release.get('title', 'no title'))
                    artist = release['artist-credit'][0]['artist']['name']
                    no_of_titles = disc_info['disc']['offset-count']
                    artist_title = artist + " " + title
                    args = {
                        'job_id': str(job.job_id),
                        'crc_id': release['id'],
                        'hasnicetitle': True,
                        'year': str(new_year),
                        'year_auto': str(new_year),
                        'title': artist_title,
                        'title_auto': artist_title,
                        'no_of_titles': no_of_titles
                    }
                    logging.info(f"CD args: {args}")
                    u.database_updater(args, job)
                    logging.debug(f"musicbrain works -  New title is {title}  New Year is: {new_year}")

                    logging.info(f"do have artwork?======{release['cover-art-archive']['artwork']}")
                    if get_cd_art(job, disc_info):
                        logging.debug("we got an art image")
                    else:
                        logging.debug("we didnt get art image")
                    music_data = artist_title

    elif 'cdstub' in disc_info:
        logging.info("Processing as a cdstub")
        process_tracks(job, disc_info['cdstub']['track-list'], is_stub=True)

        title = str(disc_info['cdstub']['title'])
        artist = disc_info['cdstub']['artist']
        no_of_titles = disc_info['cdstub']['track-count']
        # Use GNUDB year if present, otherwise empty
        new_year = disc_info['cdstub'].get('_year', '')
        artist_title = artist + " " + title
        args = {
            'job_id': str(job.job_id),
            'crc_id': disc_info['cdstub']['id'],
            'hasnicetitle': True,
            'year': new_year,
            'year_auto': new_year,
            'title': artist_title,
            'title_auto': artist_title,
            'no_of_titles': no_of_titles
        }
        logging.info(f"cdstub args: {args}")
        u.database_updater(args, job)
        logging.info("do have artwork?======No (cdstub)")
        logging.debug(f"musicbrain works, but stubbed -  New title is {artist_title}")

        music_data = artist_title

    return music_data


def check_date(release: dict) -> str:
    """Extract and normalize the release year from a MusicBrainz release dictionary."""
    if 'date' in release:
        new_year = str(release['date'])
        new_year = re.sub(r"-\d{2}-\d{2}$", "", new_year)
    else:
        new_year = ""
    return new_year


def get_title(discid: str, job) -> str:
    """
    Query MusicBrainz for the album title and artist associated with a disc ID.
    Returns a sanitized "Artist-Title" string, or "not identified" on failure.
    """

    mb.set_useragent("arm", version=str(job.arm_version), contact="https://github.com/automatic-ripping-machine")
    try:
        disc_info = mb.get_releases_by_discid(discid, includes=['artist-credits'])
        logging.debug(f"disc_info: {disc_info}")
        logging.debug(f"discid = {discid}")
        if 'disc' in disc_info:
            title = str(disc_info['disc']['release-list'][0]['title'])
            artist = str(disc_info['disc']['release-list'][0]['artist-credit'][0]['artist']['name'])
            crc_id = str(disc_info['disc']['release-list'][0]['id'])
        elif 'cdstub' in disc_info:
            title = str(disc_info['cdstub']['title'])
            artist = str(disc_info['cdstub']['artist'])
            crc_id = str(disc_info['cdstub']['id'])
        else:
            u.database_updater(False, job)
            return "not identified"

        clean_title = u.clean_for_filename(artist) + "-" + u.clean_for_filename(title)
        args = {
            'crc_id': crc_id,
            'title': str(artist + " " + title),
            'title_auto': str(artist + " " + title),
            'video_type': "Music"
        }
        u.database_updater(args, job)
        return clean_title
    except (mb.WebServiceError, KeyError):
        u.database_updater(False, job)
        return "not identified"


def get_cd_art(job, disc_info: str) -> bool:
    """
    Retrieve and store CD artwork from MusicBrainz if available.
    Returns True if artwork was found and saved, False otherwise.
    """
    try:
        if 'disc' in disc_info:
            release_list = disc_info['disc']['release-list']
            logging.debug(f"release_list: {release_list}")
            first_release_with_artwork = next(
                (release for release in release_list if release.get('cover-art-archive', {}).get('artwork') != "false"),
                None
            )
            logging.debug(f"first_release_with_artwork: {first_release_with_artwork}")

            if first_release_with_artwork is not None:
                artlist = mb.get_image_list(first_release_with_artwork['id'])
                logging.debug(f"artlist: {artlist}")

                for image in artlist["images"]:
                    if "image" in image:
                        args = {
                            'poster_url': str(image["image"]),
                            'poster_url_auto': str(image["image"])
                        }
                        u.database_updater(args, job)
                        logging.debug(f"poster_url: {args['poster_url']} poster_url_auto: {args['poster_url_auto']}")
                        return True
        return False
    except mb.WebServiceError as exc:
        u.database_updater(False, job)
        logging.error(f"get_cd_art ERROR: {exc}")
        return False


def process_tracks(job, mb_track_list: dict, is_stub=False):
    """
    Process a list of MusicBrainz tracks and store them in the database.
    Handles both stub and full metadata modes.
    """
    for (idx, track) in enumerate(mb_track_list):
        track_leng = 0
        try:
            if is_stub:
                track_leng = int(track['length'])
            else:
                track_leng = int(track['recording']['length'])
        except (ValueError, KeyError):
            logging.error("Failed to find track length")
        trackno = track.get('number', idx + 1)
        if is_stub:
            title = track.get('title', f"Untitled track {trackno}")
        else:
            title = track['recording']['title']
        u.put_track(job, trackno, track_leng, "n/a", 0.1, False, "ABCDE", title)


if __name__ == "__main__":
    # this will break our logging if it ever triggers for arm
    disc = Disc("/dev/cdrom")
    myid = get_disc_id(disc)
    logging.debug("DiscID: %s (%s)", str(myid), myid.freedb_id)
    logging.debug("URL: %s", myid.submission_url)
    logging.debug("Tracks: %s", myid.tracks)
    logging.debug("Musicbrain: %s", music_brainz(myid, None))
