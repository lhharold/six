#ifndef __SIX_CORE_H_INCLUDE__
#define __SIX_CORE_H_INCLUDE__

#include "define.h"

#include "buildsetting.h"

#if RENDER_SYS == RENDER_SYS_GL
	#include <gl/GL.h>
	#include "gl/glext.h"
#else
#endif

#include "stringutil.h"

#include "datastream.h"
#include "filestream.h"
#include "filesystem.h"

#include "renderwindow.h"

#include "rendersystem.h"

#include "root.h"

#endif //__SIX_CORE_H_INCLUDE__