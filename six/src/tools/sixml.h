#ifndef __SIX_SIXML_H_INCLUDE__
#define __SIX_SIXML_H_INCLUDE__
#include "rapidxml.hpp"
#include "rapidxml_utils.hpp"
#include "rapidxml_print.hpp"
#include "rapidxml_iterators.hpp"

namespace six {
#define CASE_SENSITIVE true
const int PARSE_FLAG = rapidxml::parse_declaration_node|rapidxml::parse_no_entity_translation;

class Sixml {
public:
	typedef rapidxml::xml_document<>	rapid_doc;
  typedef rapidxml::xml_node<>	    rapid_node;
	typedef rapidxml::xml_attribute<>	rapid_attr;

public:
	Sixml() {}
	virtual ~Sixml(){};

	virtual void* getRoot();
	virtual int	getChildCount(void* hItem, const char* childName = NULL);
	virtual bool setDocEncoding(const char* encoding);
	virtual void* getParent(void* hItem);
  virtual void* getChild(void* hItem, const char* childName = NULL);
	virtual void* getPrevItem(void* hItem, const char* childName = NULL);
	virtual void* getNextItem(void* hItem, const char* childName = NULL);

  virtual void* localItem(void* hItem, const char* pathName, bool create = false);
  virtual void* insertAfterItem(void* hItem, const char* name);
	virtual void* insertBeforeItem(void* hItem, const char* name);
  virtual void* insertChild(void* hItem, const char* name);
	virtual bool deleteItem(void* hItem);

	const char* getText(void* hItem);
	virtual const char* getName(void* hItem);
  virtual const char* getAttribute(void* hItem, const char* attrName = NULL);
	int getAttributeInt(void* hItem, const char* attrName = NULL);
	virtual int getAttributeCount(void* hItem);
	virtual const char* getAttribute(void* hItem, int index, char* name = NULL, int lenght = 0);
	virtual const char* getComment(void* hItem);

	virtual bool setName(void* hItem, const char* name);
	virtual bool setText(void* hItem, const char* text);
  virtual bool setAttribute(void* hItem, const char* attrName, const char* attrValue = NULL);
	virtual bool setAttributeInt(void* hItem, const char* attrName, int value = 0);
	virtual void* setComment(void* hItem, const char* comment = NULL);

	virtual bool load(const char* fileName);
	virtual bool loadBuff(const char* fileData);
	virtual bool save(const char* fileName);
	virtual int getXML(void* hItem, char* pBuff, int bufLen, int& nWritten);
	void* copyBranch(void* hSourceNode);
	void* copyBranchAsChild(void* hDestNode, void* hSourceNode);
private:
	bool _isNode(void* hItem);
	bool _loadFile(FILE* file);
	bool _loadBuff(const char* pBuff, int buffSize, bool bCopy);
	const char* getEncoding(const char* pBuff, int nBufferSize);
	virtual void freeXMLString(const char* xmlBuff);
private:
	rapid_doc	mDoc;
	String mEncoding;
};
}

#endif //__SIX_SIXML_H_INCLUDE__