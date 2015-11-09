#include "six.h"
using namespace six;

#if 1
int main(int argn, const char* argv[]) {
  Root* root = NEW Root();
  root->startup(true);
  root->startRun();
  return 0;
}
#else
int main(int argn, const char* argv[]) {

	FileSystem fileSystem(".");
	IDataStream* vsStream = fileSystem.open("default_vs.glsl");
	IDataStream* psStream = fileSystem.open("default_ps.glsl");

	char str[512];
	memset(str, 0, sizeof(str));
	printf("%%%%%%%%%%%%vs%%%%%%%%%%%%%%%%%\n");
	vsStream->readBuffer(str, sizeof(str));
	printf("%s\n", str);
	memset(str, 0, sizeof(str));
	psStream->readBuffer(str, sizeof(str));
	printf("%%%%%%%%%%%%ps%%%%%%%%%%%%%%%%%\n");
	printf("%s\n", str);

	SAFE_DEL(vsStream);
	SAFE_DEL(psStream);

	return 0;
}
#endif

#if 0
#include <windows.h>

#define MAX_LOADSTRING 100

// Global Variables:
HINSTANCE hInst;								// current instance
TCHAR szTitle[MAX_LOADSTRING];					// The title bar text
TCHAR szWindowClass[MAX_LOADSTRING];			// the main window class name

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

bool sExit = false;

ATOM MyRegisterClass(HINSTANCE hInstance) {
	WNDCLASSEX wcex;
	wcex.cbSize = sizeof(WNDCLASSEX);
	wcex.style			= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon			= NULL;
	wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= NULL;
	wcex.lpszClassName	= szWindowClass;
	wcex.hIconSm		= NULL;
	return RegisterClassEx(&wcex);
}

BOOL InitInstance(HINSTANCE hInstance, int nCmdShow) {
	HWND hWnd;
	hInst = hInstance; // Store instance handle in our global variable
	hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT, 0, 800, 600, NULL, NULL, hInstance, NULL);
	if (!hWnd) 
		return FALSE;
	ShowWindow(hWnd, nCmdShow);
	UpdateWindow(hWnd);

#if 0
	HDC hDC = GetDC(hWnd);
	ogl = new Oglplatform(800, 600);
	ogl->createContent(hWnd, hDC);
#endif
	return TRUE;
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
	PAINTSTRUCT ps;
	HDC hdc;
	switch (message) {
	case WM_COMMAND:
		break;
	case WM_PAINT:
		hdc = BeginPaint(hWnd, &ps);
		// TODO: Add any drawing code here...
		EndPaint(hWnd, &ps);
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		sExit = true;
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

int APIENTRY WinMain(HINSTANCE hInstance,
					   HINSTANCE hPrevInstance,
					   LPTSTR    lpCmdLine,
					   int       nCmdShow)
{
	MSG msg;
	HACCEL hAccelTable;
	strcpy(szTitle, "six");
	strcpy(szWindowClass, "six");
	MyRegisterClass(hInstance);
	if (!InitInstance(hInstance, nCmdShow)) 
		return FALSE;
	hAccelTable = LoadAccelerators(hInstance, szTitle);
	while(!sExit) {
		if(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
			if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg)) {
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			} else {
			}
		}
	}

	return (int) msg.wParam;
}
#endif
