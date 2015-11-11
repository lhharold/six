#include "glcore.h"
#include "windowevent.h"

namespace six {
  void WindowEvent::messagePump() {
#if OS_PLATFORM == OS_PLATFORM_WIN32
    MSG  msg;
    while( PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
#endif
  }
#if OS_PLATFORM == OS_PLATFORM_WIN32
  LRESULT CALLBACK WindowEvent::_WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (msg == WM_CREATE) {
      SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)(((LPCREATESTRUCT)lParam)->lpCreateParams));
      return 0;
    }
    RenderWindow* win = (RenderWindow*)GetWindowLongPtr(hWnd, GWLP_USERDATA);
    if(!win) {
      return DefWindowProc(hWnd, msg, wParam, lParam);
    }
    switch (msg)
    {
    case WM_ACTIVATE:
    {
      bool active = LOWORD(wParam) != WA_INACTIVE;
      if(active) {
        win->setActive(true);
      } else {
        if(win->isDeactivatedOnFocusChange())
          win->setActive(false);
      }
      //notify listener
    }
    break;
    case WM_SYSKEYDOWN:
      switch(wParam)
      {
      case VK_CONTROL:
      case VK_SHIFT:
      case VK_MENU: //alt
        return 0;
      }
    break;
    case WM_SYSKEYUP:
      switch(wParam)
      {
      case VK_CONTROL:
      case VK_SHIFT:
      case VK_MENU: //alt
        return 0;
      }
      break;
    case WM_SYSCHAR:
      if(wParam != VK_SPACE)
        return 0;
      break;
    case WM_ENTERSIZEMOVE:
      break;
    case WM_EXITSIZEMOVE:
      break;
    case WM_MOVE:
    case WM_DISPLAYCHANGE:
    case WM_SIZE:
      win->windowMovedOrResized();
      //notify listener;
      break;
    case WM_GETMINMAXINFO:
      ((MINMAXINFO*)lParam)->ptMinTrackSize.x = 100;
      ((MINMAXINFO*)lParam)->ptMinTrackSize.y = 100;
      break;
    case WM_CLOSE:
    {
      //notify listener;
      win->destroy();
      return 0;
    }
    }// end switch

    return DefWindowProc(hWnd, msg, wParam, lParam);
  }
#endif
}
