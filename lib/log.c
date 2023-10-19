#include "log.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>


FILE* fileER_;/// error messages
FILE* fileLEX;/// lexical analysis messages
FILE* fileSYN;/// syntatic analysis messages
FILE* fileTAB;/// symbol table
FILE* fileGEN;/// generated code
FileDestination filesOpened; /// which files will be opened. e.g. if will only do up to symbol table generation, do not open the file to generated code
FileDestination currentState; /// marks the current stage of the compilation, used for pc and pce functions

void splitFileName(const char *fullFileName, char *path, char *fileName, char *extension);

/*
 * \brief open the files specified by files2open in the directory specified by path
 * \par sets currentState to LEX
 */
void initializePrinter(const char *path, const char* baseName, FileDestination files2open) {
    char basepath[512];
    char basefileName[256];
    char baseextension[256];
    splitFileName(baseName, basepath, basefileName, baseextension);
    currentState = LEX;
    
    if (!path) { fprintf(stderr, "called initializePrinter with path == NULL"); abort(); }
    if (!baseName) { fprintf(stderr, "called initializePrinter with baseName == NULL"); abort(); }
    
    char filename[512];

    if (files2open & ER_) { 
        int ret = snprintf(filename, sizeof(filename), "%s/%s_err.txt",path, basefileName);
        if (ret < 0) { fprintf(stderr,"FAILED WRITING FILENAME _err FOR %s",baseName); abort(); }
        fileER_ = fopen(filename, "w");
    }

    if (files2open & LEX) { 
        snprintf(filename, sizeof(filename), "%s/%s_lex.txt",path, basefileName);
        fileLEX = fopen(filename, "w");
    }

    if (files2open & SYN) { 
        snprintf(filename, sizeof(filename), "%s/%s_syn.txt",path, basefileName);
        fileSYN = fopen(filename, "w");
    }

    if (files2open & TAB) { 
        snprintf(filename, sizeof(filename), "%s/%s_tab.txt",path, basefileName);
        fileTAB = fopen(filename, "w");
    }
    
    if (files2open & GEN) { 
        snprintf(filename, sizeof(filename), "%s/%s_gen.txt",path, basefileName);
        fileGEN = fopen(filename, "w");
    }
    filesOpened = files2open;
}//initializePrinter

/// closes all opened files
void closePrinter() {
    if (fileER_ != NULL) fclose(fileER_);
    if (fileLEX != NULL) fclose(fileLEX);
    if (fileSYN != NULL) fclose(fileSYN);
    if (fileTAB != NULL) fclose(fileTAB);
    if (fileGEN != NULL) fclose(fileGEN);
    
}//closePrinter

/// sets curentstate to SYN
void doneLEXstartSYN() {
    currentState = SYN;
}
/// sets curentstate to TAB
void doneSYNstartTAB() {
    currentState = TAB;
}
/// sets curentstate to GEN
void doneTABstartGEN() {
    currentState = GEN;
}

/// flushes all opened files.
void fflushc() {
    if (currentState & ER_ & filesOpened) fflush(fileER_);
    if (currentState & LEX & filesOpened) fflush(fileLEX);
    if (currentState & SYN & filesOpened) fflush(fileSYN);
    if (currentState & TAB & filesOpened) fflush(fileTAB);
    if (currentState & GEN & filesOpened) fflush(fileGEN);
}

/// prints in both CURRENT output file AND stdout
void pc(const char* format, ...) {
    
     if (NULL == format) {
         fprintf(stderr,"called pc( with NULL format!"); abort(); }
    
     char buffer[1000];
     va_list args;
     va_start(args, format);
     vsprintf(buffer, format, args);
     
     if (currentState & ER_ & filesOpened) fprintf(fileER_, "%s", buffer);
     if (currentState & LEX & filesOpened) fprintf(fileLEX, "%s", buffer);
     if (currentState & SYN & filesOpened) fprintf(fileSYN, "%s", buffer);
     if (currentState & TAB & filesOpened) fprintf(fileTAB, "%s", buffer);
     if (currentState & GEN & filesOpened) fprintf(fileGEN, "%s", buffer);
     
     fprintf(stdout,"%s", buffer);
     va_end(args);
    
}//pp

/*
 * \brief prints in CURRENT output file AND stdout AND error file (3-way)
 * \par use this to print error messages.
 */
 void pce(const char* format, ...) {
     
    if (NULL == format) {
         fprintf(stderr,"called pc( with NULL format!"); abort(); }
    
     char buffer[1000];
     va_list args;
     va_start(args, format);
     vsprintf(buffer, format, args);
     
     
     if (currentState & LEX & filesOpened) fprintf(fileLEX, "%s", buffer);
     if (currentState & SYN & filesOpened) fprintf(fileSYN, "%s", buffer);
     if (currentState & TAB & filesOpened) fprintf(fileTAB, "%s", buffer);
     if (currentState & GEN & filesOpened) fprintf(fileGEN, "%s", buffer);
     
     if (ER_ & filesOpened) fprintf(fileER_, "%s", buffer);
     
     fprintf(stdout,"%s", buffer);
     va_end(args);
    
}//ppe


/*
 * \brief prints in both chosen outputs file AND stdout
 * \par this function is intended for hardcoded control about output files. 
 * \par it does not care about the current state. It checks if the file was set to be opened before printing
 * \param destination bitflag setting which output files will be used
 * 
 */
void pp(FileDestination destination, const char* format, ...) {
        
     char buffer[1000];
     va_list args;
     va_start(args, format);
     vsprintf(buffer, format, args);
     
     if (destination & ER_ & filesOpened) fprintf(fileER_, "%s", buffer);
     if (destination & LEX & filesOpened) fprintf(fileLEX, "%s", buffer);
     if (destination & SYN & filesOpened) fprintf(fileSYN, "%s", buffer);
     if (destination & TAB & filesOpened) fprintf(fileTAB, "%s", buffer);
     if (destination & GEN & filesOpened) fprintf(fileGEN, "%s", buffer);
     
     fprintf(stdout,"%s", buffer);
     va_end(args);
    
}//pp

/**
 * \brief aux func: split fullFileName (with full path) into path/fileName/extension
 */
void splitFileName(const char *fullFileName, char *path, char *fileName, char *extension) {
    const char *lastSlash = strrchr(fullFileName, '/');
    const char *lastBackslash = strrchr(fullFileName, '\\');
    const char *lastSeparator = (lastSlash > lastBackslash) ? lastSlash : lastBackslash;

    if (lastSeparator) {
        // Extract path
        strncpy(path, fullFileName, lastSeparator - fullFileName + 1);
        path[lastSeparator - fullFileName + 1] = '\0';
    } else {
        // No path component
        path[0] = '\0';
    }

    // Extract file name and extension
    const char *fileNameStart = lastSeparator ? lastSeparator + 1 : fullFileName;
    const char *lastDot = strrchr(fileNameStart, '.');
    
    if (lastDot) {
        // Extract file name
        strncpy(fileName, fileNameStart, lastDot - fileNameStart);
        fileName[lastDot - fileNameStart] = '\0';

        // Extract extension
        strcpy(extension, lastDot + 1);
    } else {
        // No extension
        strcpy(fileName, fileNameStart);
        extension[0] = '\0';
    }
}

