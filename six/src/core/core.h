#ifndef __SIX_CORE_H_INCLUDE__
#define __SIX_CORE_H_INCLUDE__

#include "define.h"
#include "config.h"

#include "buildsetting.h"

#include "math.h"

#include "color.h"
#include "vector.h"

#if RENDER_SYS == RENDER_SYS_GL
	#include <gl/GL.h>
	#include "gl/glext.h"
#else
#endif

#include "stringutil.h"
#include "singleton.h"
#include "plugin.h"
#include "dynlib.h"
#include "dynlibmgr.h"

#include "datastream.h"
#include "filestream.h"
#include "filesystem.h"

#include "renderwindow.h"

#include "rendersystem.h"

#include "root.h"

#endif //__SIX_CORE_H_INCLUDE__