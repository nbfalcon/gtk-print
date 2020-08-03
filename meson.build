project('gtk-print', 'c', default_options : ['c_std=c89'])

gtk_dep = dependency('gtk+-3.0')
poppler_data_dep = dependency('poppler-data')
poppler_glib_dep = dependency('poppler-glib')

src = ['src/main.c', 'src/print.c', 'src/prompt_password.c']
deps = [gtk_dep, poppler_data_dep, poppler_glib_dep]

incdirs = include_directories('include')

executable('gtk-print', src, dependencies : deps, include_directories : incdirs)