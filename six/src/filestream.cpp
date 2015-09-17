#include "define.h"
#include "filestream.h"

namespace six {

	FileStream::FileStream(const char* name) 
		: IDataStream(name)
		, mFile(NULL)
		, mError(FILE_ERROR_NO_ERROR)
	{
	}

	FileStream::FileStream(const char* name, s32 size)
		: IDataStream(name)
		, mFile(NULL)
		, mSize(size)
		, mError(FILE_ERROR_NO_ERROR)
	{
	}

	FileStream::~FileStream() {
		close();
	}

	bool FileStream::open(const char* flags) {
		if(mFile != NULL) {
			mError = FILE_ERROR_OPENED;
			return false;
		}
		mFile = FOPEN(mName.c_str(), flags);
		if(mFile == NULL) {
			mError = FILE_ERROR_FAILD_OPEN;
			return false;
		}

		if(mSize < 0) {
			FSEEK((FILE*)mFile, 0, SEEK_END);
			mSize = FTELL((FILE*)mFile);
			if(mSize < 0) {
				mError = FILE_ERROR_LENGTH_ERROR;
				return false;
			}
			FSEEK((FILE*)mFile, 0, SEEK_SET);
		}
		return true;
	}

	void FileStream::close() {
		if (mFile != NULL) {
			FCLOSE((FILE*)mFile);
			mFile = NULL;
			mSize = -1;
			mError = FILE_ERROR_NO_ERROR;
		}
	}

	bool FileStream::readBool() {
		ASSERT(mFile);
		u8 val;
		FREAD(&val, 1, 1, (FILE*)mFile);
		return val != 0;
	}

	char FileStream::readChar() {
		ASSERT(mFile);
		char val;
		FREAD(&val, 1, 1, (FILE*)mFile);
		return val;
	}
	short FileStream::readShort() {
		ASSERT(mFile);
		short val;
		FREAD(&val, sizeof(val), 1, (FILE*)mFile);
		return val;
	}
	int FileStream::readInt() {
		ASSERT(mFile);
		int val;
		FREAD(&val, sizeof(val), 1, (FILE*)mFile);
		return val;
	}
	s64 FileStream::readInt64() {
		ASSERT(mFile);
		s64 val;
		FREAD(&val, sizeof(val), 1, (FILE*)mFile);
		return val;
	}
	float FileStream::readFloat() {
		ASSERT(mFile);
		float val;
		FREAD(&val, sizeof(val), 1, (FILE*)mFile);
		return val;
	}
	double FileStream::readDouble() {
		ASSERT(mFile);
		double val;
		FREAD(&val, sizeof(val), 1, (FILE*)mFile);
		return val;
	}
	u32 FileStream::readBuffer(void* buffer, int size) {
		ASSERT(mFile);
		return FREAD(buffer, 1, size, (FILE*)mFile);
	}
	u32 FileStream::readString(char* str, u32 size) {
		ASSERT(mFile);
		u32 i = 0;
		char c = 0;
		do {
			 FREAD(&c, 1, 1, (FILE*)mFile);
			 if (i < size)
				str[i] = c;
			 ++i;
		}while(c != 0);
		ASSERT(i < size);
		return i;
	}

	void FileStream::writeBool(bool val) {
		ASSERT(mFile);
		FWRITE(&val, 1, 1, (FILE*)mFile);
	}
	void FileStream::writeChar(char val) {
		ASSERT(mFile);
		FWRITE(&val, 1, 1, (FILE*)mFile);
	}
	void FileStream::writeShort(short val) {
		ASSERT(mFile);
		FWRITE(&val, sizeof(val), 1, (FILE*)mFile);
	}
	void FileStream::writeInt(int val) {
		ASSERT(mFile);
		FWRITE(&val, sizeof(val), 1, (FILE*)mFile);
	}
	void FileStream::writeInt64(s64 val) {
		ASSERT(mFile);
		FWRITE(&val, sizeof(val), 1, (FILE*)mFile);
	}
	void FileStream::writeFloat(float val) {
		ASSERT(mFile);
		FWRITE(&val, sizeof(val), 1, (FILE*)mFile);
	}
	void FileStream::writeDouble(double val) {
		ASSERT(mFile);
		FWRITE(&val, sizeof(val), 1, (FILE*)mFile);
	}
	u32 FileStream::writeBuffer(void* buffer, int size) {
		ASSERT(mFile);
		return FWRITE(buffer, 1, size, (FILE*)mFile);
	}
	u32 FileStream::writeBufferAlign(void* buffer, int size, u32 align) {
		ASSERT(mFile);
		u32 writesize = FWRITE(buffer, 1, size, (FILE*)mFile);
		if(align > 0) {
			u32 l = size & (align - 1);
			if (l > 0 && l < align) {
				writesize += FWRITE(&size, 1, align-l, (FILE*)mFile);
			}
		}
		return writesize;
	}
	u32 FileStream::writeString(const char* str) {
		ASSERT(mFile);
		return FWRITE((void*)str, 1, STRLEN(str)+1, (FILE*)mFile);
	}

	u32 FileStream::readLine(char* buff, u32 maxCount, const char* delim/* = "\n"*/) {
#if 0
		ASSERT(mFile);
		u32 i = 0;
		char c = 0;
		do {
			 FREAD(&c, 1, 1, (FILE*)mFile);
			 if (i < size)
				buff[i] = c;
			 ++i;
		}while(c != 0);
		ASSERT(i < size);
		return i;
#else
		return 0;
#endif
	}
	u32 FileStream::skipLine(const char* delim/* = "\n"*/) {
		ASSERT(mFile);
		return 0;
	}
	void FileStream::skip(s32 count) {
		ASSERT(mFile);
        FSEEK((FILE*)mFile, count, SEEK_CUR);
	}
	void FileStream::seek(u32 pos) {
		ASSERT(mFile);
        FSEEK((FILE*)mFile, static_cast<long>(pos), SEEK_SET);
	}
	u32 FileStream::tell() {
		ASSERT(mFile);
		return FTELL((FILE*)mFile);
	}
}