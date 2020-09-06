i18n = import('i18n')

prefix = get_option('prefix')
localedir = join_paths(prefix, get_option('localedir'))

add_project_arguments('-DGETTEXT_PACKAGE="@0@"'.format(meson.project_name()),
                      '-DLOCALEDIR="@0@"'.format(localedir),
                      language : 'c')
i18n.gettext(meson.project_name(), args : ['--directory=' + meson.source_root(),
             '--from-code=utf-8', '--keyword=N_'])
