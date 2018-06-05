#ifndef __SIX_STRING_H_INCLUDE__
#define __SIX_STRING_H_INCLUDE__

namespace six {

  inline bool STR_EMPTY(const char* value) {
    return value == NULL || value[0] == 0;
  }
	inline char CHR_LOW(char c) {
		return (c >= 'A' && c <= 'Z') ? c + ('a' - 'A') : c;
	}
	inline char CHR_LOWED(char c) {
		if (c >= 'A' && c <= 'Z')
			c += 'a' - 'A';
		return c;
	}
	inline bool IS_LETTER(char c) {
		return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
	}
	inline bool IS_NUMBER(char c) {
		return c >= '0' && c <= '9';
	}
	inline u32 BKDRHash(const char* p) {
		u32 seed = 131; 
		u32 hash = 0;
		while (*p) {
			hash = hash * seed + (*p++);
		}
		return (hash & 0x7FFFFFFF);
	}
	inline u32 BKDRHash(const char* p, char sp) {
		u32 seed = 131; 
		u32 hash = 0;
		while (*p && *p != sp) {
			hash = hash * seed + *p++;
		}
		return (hash & 0x7FFFFFFF);
	}
	inline static u32 BKDRHashN(const char* p, const char* e) {
		u32 seed = 131; 
		u32 hash = 0;
		while (p != e) {
			hash = hash * seed + (*p++);
		}
		return (hash & 0x7FFFFFFF);
	}
	inline u32 BKDRHashL(const char* p) {
		u32 seed = 131; 
		u32 hash = 0;
		while (*p) {
			hash = hash * seed + CHR_LOW(*p++);
		}
		return (hash & 0x7FFFFFFF);
	}
# define Hash(x) BKDRHash(x)

  class StringUtil {
  public:
    static void trim(String& str, bool left = true, bool right = true);

    static u32 split(Vector<String>& out, const String str, const String& delims = "\t\n ", u32 maxSplits = 0, bool preserveDelims = false);

    static u32 tokenise(Vector<String>&out, const String& str, const String& singleDelims = "\t\n ", const String& doubleDelims = "\"", unsigned int maxSplits = 0);

    static void toLowerCase(String& str);

    static void toUpperCase(String& str);

    static bool startsWith(const String& str, const String& pattern, bool lowerCase = true);

    static bool endsWith(const String& str, const String& pattern, bool lowerCase = true);

    static String standardisePath( const String &init, bool appendEndSeparator = true);

    static String normalizeFilePath(const char* init, bool makeLowerCase = true);

    static void splitFilename(const String& qualifiedName, String& outBasename, String& outPath);

    static void splitBaseFilename(const String& fullName, String& outBasename, String& outExtention);

    static void splitFullFilename(const String& qualifiedName, String& outBasename, String& outExtention, String& outPath);

    static bool match(const String& str, const String& pattern, bool caseSensitive = true);

    static String replaceAll(const String& source, const String& replaceWhat, const String& replaceWithWhat);

    static const String BLANK;
  };
}

#endif //__SIX_STRING_H_INCLUDE__