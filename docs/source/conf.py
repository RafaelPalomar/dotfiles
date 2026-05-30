# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'Entelequia Dotfiles'
copyright = '2026, Rafael'
author = 'Rafael'
version = '1.0'
release = '1.0.0'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

# Core extensions — always present (the minimal `entelequia-docs` Guix
# package builds the Info manual with a bare python-sphinx).
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'sphinx.ext.todo',
    'sphinx.ext.viewcode',
]

# ReadTheDocs-ready stack, mirroring ~/src/Slicer-Liver: MyST (Markdown
# ADRs + key inventory), sphinxcontrib.mermaid (the secrets decision
# tree), sphinx_design, sphinx_rtd_theme.  These are present on
# ReadTheDocs (requirements-docs.txt) and in a full local build, but NOT
# in the minimal Guix docs package.  Load them only when importable and
# degrade gracefully otherwise, so `guix home` keeps building the Info
# manual with a plain python-sphinx.
_optional_extensions = [
    'myst_parser',
    'sphinx_design',
    'sphinxcontrib.mermaid',
    'sphinx_rtd_theme',
]
_have = set()
for _ext in _optional_extensions:
    try:
        __import__(_ext)
        extensions.append(_ext)
        _have.add(_ext)
    except ImportError:
        pass

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

source_suffix = {'.rst': 'restructuredtext'}
master_doc = 'index'

if 'myst_parser' in _have:
    # Full stack: author docs in Markdown (MyST) as well as rST.
    source_suffix['.md'] = 'markdown'
    myst_enable_extensions = ['attrs_inline', 'colon_fence', 'deflist', 'linkify']
    # Header anchors so cross-document Markdown links can target headings.
    myst_heading_anchors = 6
    # Route bare ```mermaid fenced blocks to the mermaid directive.
    myst_fence_as_directive = ['mermaid']
else:
    # Minimal stack (Guix Info-manual build): no Markdown parser and no
    # mermaid extension.  Drop the .md docs from the build and register a
    # no-op `mermaid` directive so the .rst pages that use it still build.
    exclude_patterns += ['adr/*', 'keys-inventory.md']
    from docutils import nodes
    from docutils.parsers.rst import Directive, directives

    class _LiteralMermaid(Directive):
        has_content = True

        def run(self):
            text = '\n'.join(self.content)
            return [nodes.literal_block(text, text)]

    directives.register_directive('mermaid', _LiteralMermaid)

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme' if 'sphinx_rtd_theme' in _have else 'alabaster'
html_static_path = ['_static']

html_theme_options = {
    'navigation_depth': 4,
    'collapse_navigation': False,
    'sticky_navigation': True,
    'titles_only': False,
}

html_title = f'{project} {release}'
html_short_title = project

# -- Options for Texinfo output ----------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-texinfo-output

texinfo_documents = [
    ('index', 'entelequia', 'Entelequia Dotfiles Documentation',
     'Rafael', 'entelequia', 'GNU Guix-based declarative dotfiles system',
     'System Administration', True)
]

# Info directory configuration
texinfo_dir_category = 'System Administration'
texinfo_dir_description = 'Entelequia GNU Guix dotfiles system'

# -- Options for manual page output ------------------------------------------

man_pages = [
    ('index', 'entelequia', 'Entelequia Dotfiles Documentation',
     [author], 7)
]

# -- Extension configuration -------------------------------------------------

# -- Options for intersphinx extension ---------------------------------------
# https://www.sphinx-doc.org/en/master/usage/extensions/intersphinx.html#configuration

intersphinx_mapping = {
    'python': ('https://docs.python.org/3', None),
    'sphinx': ('https://www.sphinx-doc.org/en/master', None),
}

# -- Options for todo extension ----------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/extensions/todo.html#configuration

todo_include_todos = True
