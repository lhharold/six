#ifndef __SIX_SCRIPTLOADER_H_INCLUDE__
#define __SIX_SCRIPTLOADER_H_INCLUDE__

namespace six {

  class ScriptLoader {
  public:
    virtual ~ScriptLoader();

    virtual const char* getScriptPatterns() = 0;
    virtual void parseScript(IDataStream* ds) = 0;
    virtual f32 getLoaderOrder() = 0;
  };
}

#endif //__SIX_SCRIPTLOADER_H_INCLUDE__