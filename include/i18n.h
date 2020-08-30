#ifndef I18N_H
#define I18N_H

#ifdef CONFIG_ENABLE_I18N
#include <glib/gi18n.h>
#else
#define _(s) (s)
#define Q_(s) (s)
#define N_(s) (s)
#define C_(s) (s)
#define NC_(c, s) (s)
#endif

#endif