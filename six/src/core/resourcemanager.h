#ifndef __SIX_RESOURCEMANAGER_H_INCLUDE__
#define __SIX_RESOURCEMANAGER_H_INCLUDE__

namespace six {
  class Resource;
  class ManualLoader;
  class ResourceManager : public ScriptLoader {
  public:
		_AUTO_MUTEX
    ResourceManager();
    virtual ~ResourceManager();

    virtual Resource* create(const char* name, bool isManual = false, ManualLoader* loader = NULL); //params
    virtual bool createOrRetrieve(Resource*& resource, const char* name, bool isManual = false, ManualLoader* loader = NULL); //params
    virtual void setMemoryBudget(u32 bytes) {mMemoryBudget = bytes; checkUsage();}
    virtual u32 getMemoryBudget() const {return mMemoryBudget;}
    virtual u32 getMemoryUsage() const {return mMemoryUsage;}
    virtual void unload(const char* name);
    virtual void unload(Resource* res);
    virtual void unloadAll(bool reloadableOnly = true);
    virtual void reloadAll(bool reloadableOnly = true);


    virtual void checkUsage();
  protected:
    u32 mMemoryBudget;
    u32 mMemoryUsage;
  };

}

#endif //__SIX_RESOURCEMANAGER_H_INCLUDE__