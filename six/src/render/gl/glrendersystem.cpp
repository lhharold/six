#include "glcore.h"
#include "glrendersystem.h"
#include "renderwindow.h"
#include "glcontext.h"

#if RENDER_SYS == RENDER_SYS_GL
	#pragma comment(lib, "OpenGL32.lib")
	#if OS_PLATFORM == OS_PLATFORM_WIN32
		#include "gl/win32/glwin32support.h"
	#endif
#endif

static const char* USERDATA_GLCONTEXT = "GLCONTEXT";

namespace six {
	GLRenderSystem::GLRenderSystem() 
		: mFlags(0) 
		, mMainContext(NULL)
		, mActiveContext(NULL)
    , mColorWrite(-1)
    , mDepthWrite(true)
    , mStencilMask(false)
	{
		mGLSupport = getGLSupport();
	}

	GLRenderSystem::~GLRenderSystem() {
	}

	RenderWindow* GLRenderSystem::startup(bool autoWindow, const char* windowName) {
		mGLSupport->start();
		RenderWindow* window = mGLSupport->createWindow(this, autoWindow, windowName);

		RenderSystem::startup(autoWindow, windowName);
		return (RenderWindow*)window;
	}

	RenderWindow* GLRenderSystem::createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) {
		if(mRenderTargets.find(windowName) != mRenderTargets.end()) {
			ASSERT(0 && "GLRenderSystem::createWindow - Window with name already exists");
			return NULL;
		}
		RenderWindow* window = mGLSupport->newWindow(windowName, width, height, fullScreen);
		attachRenderTarget(*window);

		if(!mGLInitialised) {
			initialiseContext(window);
		}
		return window;
	}

	void GLRenderSystem::shutdown() {
		mGLSupport->stop();
		mFlags = 0;
	}

  void GLRenderSystem::setViewport(Viewport* viewport) {
		if (viewport == NULL) {
			mActiveViewport = NULL;
			setRenderTarget(NULL);
		} else if (viewport != mActiveViewport || viewport->isUpdated()) {
			RenderTarget* target;
			target = viewport->getTarget();
			setRenderTarget(target);
			mActiveViewport = viewport;

			GLsizei x, y, w, h;
			w = viewport->getRealWidth();
			h = viewport->getRealHeight();
			x = viewport->getRealLeft();
			y = viewport->getRealTop();
			if (!target->requiresTextureFlipping()) {
				y = target->getHeight() - h - y;
			}
			glViewport(x, y, w, h);
			glScissor(x, y, w, h);
			viewport->clearUpdatedFlag();

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();

      const GLdouble pi = 3.1415926535897932384626433832795;
      GLdouble fW, fH;
      //fH = tan( (45.f / 2.f) / 180.f * pi ) * 0.01;
      fH = tan( 45.f / 360.f * pi ) * 0.01;
      fW = fH * 800.f/600.f;
      glFrustum( -fW, fW, -fH, fH, 0.01, 100.f);

      glMatrixMode(GL_MODELVIEW);
      //glLoadIdentity();

#if 0
      glShadeModel(GL_SMOOTH);
      glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
      glClearDepth(1.0f);
      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LEQUAL);	
      glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
#endif
    }
  }
  void GLRenderSystem::clearFrameBuffer(u32 buffers, const Color& color /* = Color::Black */, f32 depth /* = 1.f */, u16 stencil /* = 0 */) {

		bool colorMask = (!mColorWriteR) || (!mColorWriteG) || (!mColorWriteB) || (!mColorWriteA); 
		GLbitfield flags = 0;
		if (buffers & FBT_COLOUR) {
			flags |= GL_COLOR_BUFFER_BIT;
			if (colorMask) {
				glColorMask(true, true, true, true);
			}
			glClearColor(color.r, color.g, color.b, color.a);
		}
		if (buffers & FBT_DEPTH) {
			flags |= GL_DEPTH_BUFFER_BIT;
			if (!mDepthWrite) {
				glDepthMask(GL_TRUE);
			}
			glClearDepth(depth);
		}
		if (buffers & FBT_STENCIL) {
			flags |= GL_STENCIL_BUFFER_BIT;
			glStencilMask(0xFFFFFFFF);
			glClearStencil(stencil);
		}
		GLboolean scissorTestEnabled = glIsEnabled(GL_SCISSOR_TEST);
		if (!scissorTestEnabled) {
			glEnable(GL_SCISSOR_TEST);
		}
    GLint viewport[4] = {0, 0, 0, 0};
    GLint scissor[4] = {0, 0, 0, 0};
		glGetIntegerv(GL_VIEWPORT, viewport);
		glGetIntegerv(GL_SCISSOR_BOX, scissor);
		bool scissorBoxDifference = viewport[0] != scissor[0] || viewport[1] != scissor[1] || viewport[2] != scissor[2] || viewport[3] != scissor[3];
		if (scissorBoxDifference) {
			glScissor(viewport[0], viewport[1], viewport[2], viewport[3]);
		}

		glClear(flags);
		if (scissorBoxDifference) {
			glScissor(scissor[0], scissor[1], scissor[2], scissor[3]);
		}
		if (!scissorTestEnabled) {
			glDisable(GL_SCISSOR_TEST);
		}
		if (!mDepthWrite && (buffers & FBT_DEPTH)) {
			glDepthMask( GL_FALSE );
		}
		if (colorMask && (buffers & FBT_COLOUR)) {
			glColorMask(mColorWriteR, mColorWriteG, mColorWriteB, mColorWriteA);
		}
		if (buffers & FBT_STENCIL) {
			glStencilMask(mStencilMask);
		}
    glLoadIdentity();

    glTranslatef(-1.5f,0.0f,-6.0f);	
    glBegin(GL_TRIANGLES);							// 绘制三角形
    glVertex3f( 0.0f, 1.0f, 0.0f);					// 上顶点
    glVertex3f(-1.0f,-1.0f, 0.0f);					// 左下
    glVertex3f( 1.0f,-1.0f, 0.0f);					// 右下
    glEnd();								// 三角形绘制结束
    glTranslatef(3.0f,0.0f,0.0f);						// 右移3单位
    glBegin(GL_QUADS);							//  绘制正方形
    glVertex3f(-1.0f, 1.0f, 0.0f);					// 左上
    glVertex3f( 1.0f, 1.0f, 0.0f);					// 右上
    glVertex3f( 1.0f,-1.0f, 0.0f);					// 左下
    glVertex3f(-1.0f,-1.0f, 0.0f);					// 右下

    glEnd();
  }
  void GLRenderSystem::setRenderTarget(RenderTarget* target) {
    if(mActiveRenderTarget) {
			//mRTTManager->unbind(mActiveRenderTarget);
    }
		mActiveRenderTarget = target;
		if (target) {
			GLContext *newContext = NULL;
			target->getUserData(USERDATA_GLCONTEXT, &newContext);
			if(newContext && mActiveContext != newContext) {
				switchContext(newContext);
			}

			//GLDepthBuffer *depthBuffer = static_cast<GLDepthBuffer*>(target->getDepthBuffer());
			//if( target->getDepthBufferPool() != DepthBuffer::POOL_NO_DEPTH &&
			//	(!depthBuffer || depthBuffer->getGLContext() != mCurrentContext ) )
			//{
			//	//Depth is automatically managed and there is no depth buffer attached to this RT
			//	//or the Current context doesn't match the one this Depth buffer was created with
			//	setDepthBufferFor( target );
			//}

			// Bind frame buffer object
			//mRTTManager->bind(target);

			//if (GLEW_EXT_framebuffer_sRGB) {
			//	if (target->isHardwareGammaEnabled()) {
			//		glEnable(GL_FRAMEBUFFER_SRGB_EXT);
			//	} else {
			//		glDisable(GL_FRAMEBUFFER_SRGB_EXT);
			//	}
			//}
		}

  }
	//////////////////////////////////////////////////////////////////////////
	void GLRenderSystem::initialiseContext(RenderWindow* window) {
    mMainContext = NULL;
		window->getUserData("GLCONTEXT", &mMainContext);
		mActiveContext = mMainContext;
		if(mActiveContext)
			mActiveContext->setCurrent();
		mGLSupport->initialiseExtensions();
	}
  void GLRenderSystem::unregisterContext(GLContext *context) {
    if(mActiveContext == context) {
      if(mActiveContext != mMainContext) {
        switchContext(mMainContext);
      } else {
        mActiveContext->endCurrent();
        mActiveContext = NULL;
        mMainContext = NULL;
      }
    }
  }
  void GLRenderSystem::switchContext(GLContext* context) {
  }
  void GLRenderSystem::_setColourBufferWriteEnabled(bool r, bool g, bool b, bool a) {
    glColorMask(r, g, b, a);
    mColorWriteR = r;
    mColorWriteG = g;
    mColorWriteB = b;
    mColorWriteA = a;
  }
}