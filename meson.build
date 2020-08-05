project('gtk-print', 'c', default_options : ['c_std=c89'],
                          license : 'MIT', meson_version : '>=0.55.0')

subdir('po') # i18n
add_project_arguments('-DDATADIR="@0@"'.format(get_option('datadir')),
                      language : 'c')

if get_option('enable-fork')
   add_project_arguments('-DCONFIG_ENABLE_FORK=1', language : 'c')
endif
if get_option('enable-getpass')
   add_project_arguments('-DCONFIG_ENABLE_GETPASS=1', language : 'c')
endif
if get_option('use-strdup')
   add_project_arguments('-DCONFIG_USE_STRDUP=1', '-D_POSIX_C_SOURCE=200809L',
                         language : 'c' )
endif

gtk_dep = dependency('gtk+-3.0')
poppler_data_dep = dependency('poppler-data')
poppler_glib_dep = dependency('poppler-glib')

src = ['src/main.c', 'src/print.c', 'src/prompt_password.c']
deps = [gtk_dep, poppler_data_dep, poppler_glib_dep]

incdirs = include_directories('include')

executable('gtk-print', src, dependencies : deps, include_directories : incdirs)