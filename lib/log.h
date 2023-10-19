#ifndef VARIABLEPRINTER_H
#define VARIABLEPRINTER_H


/// bitmask to select output files
typedef enum fileDestination {
    ER_ = 0x1, // NOT STDERR!!! JUST A FILE TO STORE YOUR ERR MSGS!!
    LEX = 0x2, // lexical 
    LER = 0x3, // LEX AND ERR
    SYN = 0x4, // syntatic analisis
    SER = 0x5,
    TAB = 0x8,  // symbol table
    TER = 0x9,
    GEN = 0x10, // code generation
    GER = 0x11,
    UP2SYN = 0x7, // ER + LEX + SYN
    UP2TAB = 0xF, // ER + LEX + SYN + TAB
    LOGALL = 0x1F, // ER + LEX + SYN + TAB + GEN
} FileDestination; 

void initializePrinter(const char *path, const char* baseName, FileDestination files2open) ;
void pp(FileDestination destination, const char* format, ...);
void doneLEXstartSYN() ;
void doneSYNstartTAB() ;
void doneTABstartGEN() ;
void pc(const char* format, ...) ;
void pce(const char* format, ...) ;
void fflushc();

void closePrinter();

#endif  // VARIABLEPRINTER_H
