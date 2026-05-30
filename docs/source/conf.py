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

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'sphinx.ext.todo',
    'sphinx.ext.viewcode',
    # ReadTheDocs-ready stack, mirroring ~/src/Slicer-Liver (ADR-0017
    # there) so the cognitive overhead of moving between repos is low:
    # MyST lets ADRs + the key inventory be authored as Markdown,
    # sphinxcontrib.mermaid renders the secrets decision tree, and
    # sphinx_rtd_theme is the published theme.
    'myst_parser',
    'sphinx_design',
    'sphinxcontrib.mermaid',
    'sphinx_rtd_theme',
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# Author docs in either reStructuredText or Markdown (MyST).  The
# legacy pages are .rst; new key/secret-management docs and the ADR
# ledger are .md.
source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}
master_doc = 'index'

# -- MyST configuration ------------------------------------------------------

myst_enable_extensions = [
    'attrs_inline',
    'colon_fence',
    'deflist',
    'linkify',
]
# Generate header anchors so cross-document Markdown links can target
# sub-section headings without manual anchor tags.
myst_heading_anchors = 6
# Route bare ```mermaid fenced blocks to the mermaid directive (this is
# also what GitHub renders natively).
myst_fence_as_directive = ['mermaid']

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'
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
