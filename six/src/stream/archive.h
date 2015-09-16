#ifndef __SIX_ARCHIVE_H_INCLUDE__
#define __SIX_ARCHIVE_H_INCLUDE__

namespace six {
	class IDataStream;
	class Archive {
	public:
		Archive(const char* name) : mName(name), mReadonly(true) {}
		virtual ~Archive() {}
		virtual bool isCaseSensitive() const = 0;
		virtual void load() = 0;
		virtual void unload() = 0;
		virtual IDataStream* open(const char* filename, bool readonly = true) const = 0;
		virtual IDataStream* create(const char* filename) const {
			ASSERT("Archive::create  This archive does not support creation of files.");
			return NULL;
		}
		virtual void remove(const char* filename) const {
			ASSERT("Archive::remove  This archive does not support removal of files.");
		}
		virtual bool exists(const char* filename) = 0;
		virtual u32 getModifiedTime(const char* filename) = 0;

		const char* getName() const {return mName.c_str();}
		bool isReadonly()const {return mReadonly;}
	protected:
		String mName;
		bool mReadonly;
#if 0
		virtual void*	getHandle() = 0;
		virtual u32		getLength() = 0;
		virtual u32		getCurPos() = 0;
		virtual void	setCurPos(s32 pos) = 0;
		virtual void	seekOffset(s32 offset) = 0;

		virtual char	readChar() = 0;
		virtual short	readShort() = 0;
		virtual int		readInt() = 0;
		virtual s64		readInt64() = 0;
		virtual float	readFloat() = 0;
		virtual double	readDouble() = 0;
		virtual int		readBuffer(void* buffer, int size) = 0;
		virtual int		readString(char* str, int size) = 0;

		virtual void	writeChar(char val) = 0;
		virtual void	writeShort(short val) = 0;
		virtual void	writeInt(int val) = 0;
		virtual void	writeInt64(s64 val) = 0;
		virtual void	writeFloat(float val) = 0;
		virtual void	writeDouble(double val) = 0;
		virtual void	writeBuffer(void* buffer, int size) = 0;
		virtual void	writeBufferAlign(void* buffer, int size, int align) = 0;
		virtual void	writeString(const char* str) = 0;
#endif
	};
}

#endif //__SIX_ARCHIVE_H_INCLUDE__