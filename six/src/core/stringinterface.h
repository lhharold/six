#ifndef __SIX_STRINGINTERFACE_H_INCLUDE__
#define __SIX_STRINGINTERFACE_H_INCLUDE__

namespace six {
  enum ArgType {
    ARGT_BOOL,
    ARGT_I8,
    ARGT_U8,
    ARGT_I16,
    ARGT_U16,
    ARGT_I32,
    ARGT_U32,
    ARGT_I64,
    ARGT_U64,
    ARGT_F32,
    ARGT_F64,
    ARGT_STR,
    ARGT_VEC2,
    ARGT_VEC3,
    ARGT_VEC4,
    ARGT_MAT3,
    ARGT_MAT4,
    ARGT_QUAT,
    ARGT_COLOR,
  };
  class ArgDef {
  public:
    String name;
    String desc;
    ArgType type;
    ArgDef(const char* _name, const char* _desc, ArgType _type) 
      : name(_name), desc(_desc), type(_type) {}
  };
  typedef Vector<ArgDef> ArgList;

  class ArgCmd {
  public:
    virtual String get(void* target) = 0;
    virtual void set(void* target, const char* val) = 0;
    virtual ~ArgCmd() {}
  };
  typedef Map<u32, ArgCmd*> ArgCmdMap;

  class ArgDictionary {
  public:
    void addArg(const ArgDef& argDef, ArgCmd* argCmd) {
      mArgList.push_back(argDef);
      mArgCmdMap[Hash(argDef.name.c_str())] = argCmd;
    }
    ArgList* getArgs(void) {
      return &mArgList;
    }
    const ArgList* getArgs(void) const {
      return &mArgList;
    }
  protected:
    ArgList mArgList;
    ArgCmdMap mArgCmdMap;

    ArgCmd* _getCommand(const char* name) {
      ArgCmdMap::iterator i = mArgCmdMap.find(Hash(name));
      if(i != mArgCmdMap.end())
        return i->second;
      return NULL;
    }

    friend class StringInterface;
  };
  typedef Map<u32, ArgDictionary> ArgDictionaryMap;

  class StringInterface {
  public:
    static void cleanupDictionary() {
      _LOCK_MUTEX(sDictMutex);
      sDictionary.clear();
    }
    ArgDictionary* getArgDictionary() {
      return mArgDict;
    }
    virtual void copyArgs(StringInterface* dest) {
      const ArgDictionary* dict = getArgDictionary();
      if (dict) {
        for (ArgList::const_iterator i = dict->mArgList.begin(); i != dict->mArgList.end(); ++i) {
          dest->setArg(i->name.c_str(), getArg(i->name.c_str()).c_str());
        }
      }
    }
    virtual String getArg(const char* name) {
      ArgDictionary* dict = getArgDictionary();
      if(dict) {
        ArgCmd* cmd = dict->_getCommand(name);
        if(cmd)
          return cmd->get(this);
      }
      return "";
    }
    ArgList* getArgs() {
      ArgDictionary* dict = getArgDictionary();
      if (dict)
        return dict->getArgs();
      return NULL;
    }
    virtual bool setArg(const char* name, const char* val) {
      ArgDictionary* dict = getArgDictionary();
      if (dict) {
        ArgCmd* cmd = dict->_getCommand(name);
        if (cmd) {
          cmd->set(this, val);
          return true;
        }
      }
      return false;
    }
    virtual void setArgs(const char** name, const char** val, u32 count) {
      for(u32 i = 0; i < count; i++) {
        setArg(name[i], val[i]);
      }
    }
  protected:
    bool createArgDictionary(const char* name) {
			_LOCK_MUTEX(sDictMutex);
      mArgDictName = name;
      u32 hashName = Hash(name);
      ArgDictionaryMap::iterator i = sDictionary.find(hashName);
      if(i == sDictionary.end()) {
        mArgDict = &sDictionary.insert(std::make_pair(hashName, ArgDictionary())).first->second;
      } else {
        mArgDict = &i->second;
        return false;
      }
    }
  private:
    String mArgDictName;
    ArgDictionary* mArgDict;
    static ArgDictionaryMap sDictionary;
		_STATIC_MUTEX(sDictMutex);
  };

}

#endif //__SIX_STRINGINTERFACE_H_INCLUDE__