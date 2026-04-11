% This is the nfr.csl class file for NFR applicatios
% the use of this class requires XeLaTex

% Preamble. This is needed
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{nfr}[2022/10/18 NFR proposal LaTeX class]

% This is used to inherit from article and forward the options
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

\LoadClass{article}

%===============================================================================
% COLORS
%===============================================================================
\RequirePackage{xcolor}

\definecolor{sectioncolor}{HTML}{8388fc}

%===============================================================================
% FONTS SETTINGS
%===============================================================================
\RequirePackage{fontspec}

% Dosis — Guix font-dosis v1.7 uses "Book" as the regular weight
\newfontfamily\Dosis[Ligatures=TeX]{Dosis-Book}
\newfontfamily\DosisLight[Ligatures=TeX]{Dosis-Light}
\newfontfamily\DosisExtraLight[Ligatures=TeX]{Dosis-ExtraLight}
\newfontfamily\DosisSemiBold[Ligatures=TeX]{Dosis-SemiBold}
% SciFly Sans (freeware, installed via font-sciflycore-sans Guix package)
\newfontfamily\SciFly[Ligatures=TeX]{SciFly-Sans}
% Gentium v7 body font (SIL, installed via font-sil-gentium Guix package)
% Extension=.ttf prevents xdvipdfmx from resolving to the WOFF web-font variants
\setromanfont[
  BoldFont={Gentium-Bold},
  ItalicFont={Gentium-Italic},
  BoldItalicFont={Gentium-BoldItalic},
  Extension=.ttf
]{Gentium-Regular}

%===============================================================================
% SECTIONS FORMATTING
%===============================================================================
\RequirePackage{titlesec}

\titleformat{\section} {\color{sectioncolor}\Large\DosisSemiBold} {\thesection.}{0.5em}{}
\titleformat{\subsection} {\color{sectioncolor}\DosisSemiBold} {\thesubsection.}{0.5em}{}
\titleformat{\subsubsection} {\color{sectioncolor}\DosisSemiBold} {\thesubsubsection.}{0.5em}{}

%===============================================================================
% HYPERLINKS
%===============================================================================
\RequirePackage{hyperref}

\hypersetup{
colorlinks=true,
linkcolor=sectioncolor,
filecolor=magenta,
urlcolor=cyan,
citecolor=blue
}

%===============================================================================
% HYPERLINKS
%===============================================================================
\RequirePackage{subcaption}
\DeclareCaptionFormat{custom}
{%
  \rule[1ex]{\textwidth}{0.5pt}
  \textbf{#1#2}\textit{\small #3}
}
\captionsetup{format=custom}

%===============================================================================
% CUSTOM COMMANDS
%===============================================================================
\newcommand{\tag}[2]{\fbox{\footnotesize\DosisSemiBold #1}\phantomsection \label{#2}}
\newcommand{\tagondescription}[2]{\fbox{\footnotesize\DosisLight #1}\phantomsection \label{#2}}
\newcommand{\tagref}[1]{\fbox{\footnotesize\Dosis \hyperref[#1]{#1} \normalfont}}
\newcommand{\tagrefondescription}[1]{\fbox{\footnotesize\DosisExtraLight \hyperref[#1]{#1} \normalfont}}