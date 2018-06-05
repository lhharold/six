#include "core.h"
#include "renderwindow.h"

namespace six {
  RenderWindow::RenderWindow() 
    : RenderTarget()
    , mFullScreen(false)
    , mLeft(-1)
    , mTop(-1) 
    , mAutoDeactivatedOnFocusChange(true)
  {
  }
}
