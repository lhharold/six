#include "core.h"
namespace six {
  void* Sixml::getRoot() {
    return (rapid_node*)&mDoc;
  }

  int	Sixml::getChildCount(void* hItem, const char* childName) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    rapid_node* pChild = pNode->first_node(childName, 0, CASE_SENSITIVE);

    int nCount = 0;
    for(; pChild; pChild = pChild->next_sibling(childName, 0, CASE_SENSITIVE)) {
      if(_isNode(pChild))
        nCount ++;
    }

    return nCount;
  }

  bool Sixml::setDocEncoding(const char* encoding) {
    mEncoding = encoding;
    return true;
  }

  void* Sixml::getParent(void* hItem) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    return pNode->parent();
  }

  void* Sixml::getChild(void* hItem, const char* childName) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    rapid_node*	pChild = (rapid_node*)pNode->first_node(childName, 0, CASE_SENSITIVE);

    if(!pChild)
      return NULL;
    else if(_isNode(pChild))
      return pChild;
    else 
      return getNextItem(pChild, childName);
  }

  void* Sixml::getPrevItem(void* hItem, const char* childName) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    do {
      pNode = pNode->previous_sibling(childName, 0, CASE_SENSITIVE);
    } while(pNode && !_isNode(pNode));
    return pNode;
  }

  void* Sixml::getNextItem(void* hItem, const char* childName) {
    if(hItem == NULL) return NULL;

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->parent() == NULL) return NULL;

    do {
      pNode = pNode->next_sibling(childName, 0, CASE_SENSITIVE);
    } while(pNode && !_isNode(pNode));

    return pNode;
  }

  void* Sixml::localItem(void* hItem, const char* pathName, bool create) {
    if(hItem == NULL) hItem = getRoot();
    if(!hItem)
      return NULL;

    String strPathName = pathName;
    if(strPathName.size() && *(strPathName.end() - 1) != '\\')
      strPathName += "\\";

    int nPath = 0;
    void* hItemSave = NULL;
    while ((nPath = (int)strPathName.find('\\')) != -1) {
      strPathName[nPath] = 0;

      if(create)
        hItemSave = hItem;

      hItem = getChild(hItem, strPathName.c_str());

      if(!hItem) {
        if(create)
          hItem = insertChild(hItemSave, strPathName.c_str());
        else
          break;
      }

      strPathName = &strPathName[nPath + 1];
    }
    return (strPathName.size() > 0 ? NULL : hItem);
  }

  void* Sixml::insertAfterItem(void* hItem, const char* name) {
    if(hItem == NULL) return NULL;

    rapid_node* pNode = (rapid_node*)hItem;
    rapid_node* pNodeParent = pNode->parent();

    if(pNodeParent == NULL) return NULL;

    rapid_node* pChild = mDoc.allocate_node(rapidxml::node_element, mDoc.allocate_string(name));
    // rapidxml`s insert_node inserts before node
    pNode = pNode->next_sibling();
    pNodeParent->insert_node( pNode, pChild );
    return pChild;
  }

  void* Sixml::insertBeforeItem(void* hItem, const char* name) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    rapid_node* pChild = mDoc.allocate_node(rapidxml::node_element, mDoc.allocate_string(name));

    rapid_node* pNodeParent = (rapid_node*)getParent(hItem);
    if( !pNodeParent )
      pNodeParent = &mDoc;

    // rapidxml`s insert_node inserts before node
    pNodeParent->insert_node( pNode, pChild );
    return pChild;
  }

  void* Sixml::insertChild(void* hItem, const char* name) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    rapid_node* pChild = mDoc.allocate_node(rapidxml::node_element, mDoc.allocate_string(name));
    pNode->append_node(pChild);
    return pChild;
  }

  bool Sixml::deleteItem(void* hItem) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    pNode->remove_all_attributes();
    pNode->remove_all_nodes();

    rapid_node* pParent = pNode->parent();
    if(!pParent) {
      mDoc.remove_all_attributes();
      mDoc.remove_all_nodes();
    }
    else
      pParent->remove_node(pNode);
    return true;
  }

  const char* Sixml::getText(void* hItem) {
    if(hItem == NULL) return NULL;

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->parent() == NULL) return NULL;

    switch(pNode->type()) {
      case rapidxml::node_element:
        for(pNode = pNode->first_node(); pNode; pNode = pNode->next_sibling()) {
          if(pNode->type() == rapidxml::node_data)
            return pNode->value();
        }
        break;
    }
    return NULL;
  }

  const char* Sixml::getName(void* hItem) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    return pNode->name();
  }

  const char* Sixml::getAttribute(void* hItem, const char* attrName) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->type() != rapidxml::node_element)
      return NULL;

    rapid_attr* pAttr = pNode->first_attribute(attrName, 0, CASE_SENSITIVE);
    if(pAttr)
      return pAttr->value();

    return NULL;
  }

  int Sixml::getAttributeInt(void* hItem, const char* attrName) {
    const char* ret = getAttribute(hItem, attrName);
    if(ret)
      return atoi(ret);
    return 0;		
  }

  int Sixml::getAttributeCount(void* hItem) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->type() != rapidxml::node_element)
      return 0;

    int nCount = 0;
    rapid_attr* pAttr = pNode->first_attribute();

    while(pAttr) {
      nCount++;
      pAttr = pAttr->next_attribute();
    }
    return nCount;
  }

  const char* Sixml::getAttribute(void* hItem, int index, char* name, int lenght) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->type() != rapidxml::node_element)
      return NULL;

    rapid_attr* pAttr = pNode->first_attribute();
    while(pAttr) {
      if(index == 0) {
        if(name)
          MEMCPY(name, pAttr->name(), lenght);
        return pAttr->value();
      }

      index--;
      pAttr = pAttr->next_attribute();
    }
    return NULL;
  }

  const char* Sixml::getComment(void* hItem) {
    if(hItem == NULL) return NULL;

    rapid_node* pNode = (rapid_node*)hItem;
    pNode = pNode->previous_sibling();

    if(!pNode)
      return NULL;

    if(pNode->type() == rapidxml::node_comment)
      return pNode->value();
    else
      return NULL;
  }

  bool Sixml::setName(void* hItem, const char* name) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    if( pNode->type() != rapidxml::node_element )
      return false;
    pNode->name(mDoc.allocate_string(name));
    return true;
  }

  bool Sixml::setText(void* hItem, const char* text) {
    if(hItem == NULL) return NULL;

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->type() != rapidxml::node_element)
      return false;

    if(!pNode->parent())
      return false;

    const char* _text = mDoc.allocate_string(text);
    rapid_node* pChild = pNode->first_node();

    for( ; pChild; pChild = pChild->next_sibling() ) {
      if(pChild->type() == rapidxml::node_data) {
        pChild->value(_text);
        return true;
      }
    }

    if(!pChild) {
      rapid_node* pTextNode = mDoc.allocate_node(rapidxml::node_data);
      pTextNode->value(_text);
      pNode->append_node(pTextNode);
    }
    return true;
  }

  bool Sixml::setAttribute(void* hItem, const char* attrName, const char* attrValue) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    if(pNode->type() != rapidxml::node_element)
      return false;

    rapid_attr* pAttr = pNode->first_attribute(attrName, 0, CASE_SENSITIVE);
    if( pAttr ) {
      if(attrValue)
        pAttr->value(mDoc.allocate_string(attrValue));
      else
        pNode->remove_attribute(pAttr);
    } else {
      if(attrValue) {
        pAttr = mDoc.allocate_attribute(mDoc.allocate_string(attrName), mDoc.allocate_string(attrValue));
        pNode->append_attribute(pAttr);
      }
    }

    return true;
  }

  bool Sixml::setAttributeInt(void* hItem, const char* attrName, int value) {
    char tmp[128];
    SPRINTF(tmp, "%d", attrName);
    return setAttribute(hItem, attrName, tmp);
  }

  void* Sixml::setComment(void* hItem, const char* comment) {
    if(hItem == NULL) hItem = getRoot();

    rapid_node* pNode = (rapid_node*)hItem;
    rapid_node* pNodePre = pNode->previous_sibling();
    rapid_node* pComment = NULL;

    if(pNodePre && pNodePre->type() == rapidxml::node_comment)
      pComment = pNodePre;

    if(pComment) {// 修改或者删除
      if(comment) {
        pComment->value(mDoc.allocate_string(comment));
        return pComment; 
      } else {
        deleteItem(pNodePre);
      }
      return NULL;
    }

    if(comment) {
      pComment = mDoc.allocate_node(rapidxml::node_comment);
      pComment->value(mDoc.allocate_string(comment));

      rapid_node* pParent = pNode->parent();
      if(!pParent)
        pParent = &mDoc;

      pParent->insert_node(pNode, pComment);
      return pComment;
    }
    return NULL;
  }

  bool Sixml::load(const char* fileName) {
    mDoc.clear();

    FILE* file = fopen(fileName, "rb");
    if(file) {
      bool bResult = _loadFile( file );
      fclose(file);
      return bResult;
    }
    return false;
  }
  bool Sixml::loadBuff(const char* fileData) {
    mDoc.clear();
    return _loadBuff(fileData, (int)strlen(fileData), true);
  }

  bool Sixml::save(const char* fileName) {
    //String s;
    //print(std::back_inserter(s), mDoc, 0);
    std::ofstream out(fileName);
    out << mDoc;
    return true;
  }

  int Sixml::getXML(void* hItem, char* pBuff, int bufLen, int& nWritten) {
    if(hItem == NULL) hItem = getRoot();
    rapid_node* pNode = (rapid_node*)hItem;
    String s;
    print(back_inserter(s), *pNode, 0);
    const char* strXML = s.c_str();
    int length = (int)s.length()+1;
    nWritten = 0;
    if(!pBuff || bufLen == 0) {
      return length;
    }
    if(bufLen < length) {
      MEMCPY(pBuff, strXML, bufLen);
      nWritten = bufLen;
      return length;
    }
    MEMCPY(pBuff, strXML, length);
    nWritten = length;
    return 0;
  }

  void* Sixml::copyBranch(void* hSourceNode) {
    return NULL;
  }

  void* Sixml::copyBranchAsChild(void* hDestNode, void* hSourceNode) {
    if(hSourceNode == NULL) return NULL;
    if(hDestNode == NULL) hDestNode = getRoot();

    rapid_node* pDest = (rapid_node*)hDestNode;
    rapid_node* pSource = (rapid_node*)hSourceNode;

    rapid_node* pChild = mDoc.clone_node( pSource );
    pDest->append_node(pChild);
    return pChild;
  }

  bool Sixml::_isNode(void* hItem) {
    rapid_node* pNode = (rapid_node*)hItem;
    switch(pNode->type())
    {
    case rapidxml::node_document:
    case rapidxml::node_element:
      return true;
    }
    return false;
  }

  bool Sixml::_loadFile(FILE* file) {
    // Get the file size, so we can pre-allocate the string. HUGE speed impact.
    long length = 0;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);

    if(length == 0)
      return false;

    char* pBuffer = mDoc.allocate_string(NULL, length+1);
    if(!pBuffer)
      return false;
    if(fread(pBuffer, length, 1, file) != 1) 
      return false;

    pBuffer[length] = 0;
    //真实长度+1
    length = length + 1;
    return _loadBuff(pBuffer, length, true);
  }

  bool Sixml::_loadBuff(const char* pBuff, int buffSize, bool bCopy) {
    char* strBuff = NULL;
    if(bCopy) {
      strBuff = mDoc.allocate_string(NULL, buffSize);
      MEMCPY(strBuff, pBuff, buffSize);
    } else {
      strBuff = (char*)pBuff;
    }
    mDoc.parse<PARSE_FLAG>(strBuff);
    return true;
  }

  const char* Sixml::getEncoding(const char* pBuff, int nBufferSize) {
    char * pdataBuffer = NULL;
    const char* encoding = NULL;
    do {
      rapid_doc xdAnsii;
      pdataBuffer = NEW char[nBufferSize];
      MEMCPY(pdataBuffer, pBuff, nBufferSize);
      xdAnsii.parse<PARSE_FLAG>(pdataBuffer);
      rapid_node* pxnEncode = xdAnsii.first_node();
      if(!pxnEncode) break;

      rapid_attr * pxaEncoding = pxnEncode->first_attribute("encoding", 0, false);
      if(!pxaEncoding) break;

      encoding = pxaEncoding->value();
    }while(false);
    SAFE_DEL(pdataBuffer);
    return encoding ;
  }
  void Sixml::freeXMLString(const char* xmlBuff) {
  }
}