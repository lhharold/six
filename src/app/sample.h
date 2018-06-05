#include "six.h"
#include <iostream>
using namespace six;

class Sample {
public:
  Sample() {
    mRoot = NULL;
    mWindow = NULL;
    mSceneMgr = NULL;

    mContentSetup = false;
  }
  virtual ~Sample() {}

  void startup() {
    _setup();
  }
  void run() { 
    ASSERT(mRoot);
    mRoot->run();
  }
  void shutdown() {
    _shutdown();
  }

  six::SceneManager* getSceneMgr() {return mSceneMgr;}
  virtual void createSceneMgr() {mSceneMgr = mRoot->createSceneManager();}
  virtual void setupView() {
    mCamera = mSceneMgr->createCamera("MainCamera");
    mViewport = mWindow->addViewport(mCamera);
    mCamera->setAspectRatio(mViewport->getWidth() / mViewport->getHeight());
    mCamera->setNearClipDistance(0.1f);
    mCamera->setFarClipDistance(1000.f);
  }
  virtual void setupContent() {}
  virtual void cleanupContent() {}

  virtual void _setup() {
    mRoot = NEW Root();
    mWindow = mRoot->startup(true);

    createSceneMgr();
    setupView();
    //load resource
    setupContent();
    mContentSetup = true;
  }
  virtual void _shutdown() {
    if(mContentSetup)
      cleanupContent();
    if(mSceneMgr)
      mSceneMgr->clearScene();
    mContentSetup = false;
    //unload resource
    if(mSceneMgr)
      mRoot->destorySceneManager(mSceneMgr);
    mSceneMgr = NULL;
  }
protected:
  six::Root* mRoot;
  six::SceneManager* mSceneMgr;
  six::Camera* mCamera;
  six::Viewport* mViewport;
  six::RenderWindow* mWindow;


  bool mContentSetup;
};