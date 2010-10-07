	/*
	 *  <ListFilesRPosix.c>
	 *    Copyright 2010 James Cook - All Rights Reserved.
	 */


#include <dirent.h>
#include "HsFFI.h"

#ifndef NULL
#define NULL = ((void *) 0)
#endif

HsBool isDir(struct dirent *entry) {
    return (entry && entry->d_type == DT_DIR)
        ? HS_BOOL_TRUE : HS_BOOL_FALSE;
}

HsBool getName(struct dirent *entry, char **name, int *namelen) {
    if (entry) {
        *namelen = entry->d_namlen;
        *name = entry->d_name;
    }
    
    return entry ? HS_BOOL_TRUE : HS_BOOL_FALSE;
}