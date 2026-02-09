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
]

templates_path = ['_templates']
exclude_patterns = []

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
html_static_path = ['_static']

html_theme_options = {
    'description': 'GNU Guix-based declarative dotfiles system',
    'github_user': 'rafael',
    'github_repo': 'dotfiles',
    'fixed_sidebar': True,
}

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
