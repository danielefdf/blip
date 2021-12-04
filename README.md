# blip
A command-line tool for visualizing fixed format files from mainframe.

On the mainframe it may happen to come across fixed format files, i.e. files in which data is written in this way:

    01NAME1      SURNAME101/01/0001
    02NAME2      SURNAME202/01/0001
    03NAME3      SURNAME303/01/0001

Everything is simple until you find files in which the records are defined differently (see CoBOL REDEFINE) according to one or more reference fields: in this case it is not easy to read the file, especially if there are many redefinitions and many records.

This tool allows you to interpret the data, given the reference CoBOL structure.

## use of the tool

For example, let's define the data and the reference CoBOL copy as follows:

- data

    ```
    a01xyz00abcdef.
    a02xyz10abbccc.
    a03xyz21aa@@@b.
    a04xyz22@@@@@@.
    a05xyz30aaab@@.
    a06xyz10deefff.
    a07xyz21cc@@@d.
    a08xyz22@@@@@@.
    a09xyz10ghhiii.
    a10xyz30cccd@@.
    a11xyz30eeef@@.
    a12xyz30gggh@@.
    a13xyz30iiij@@.
    a14xyz90abcdef.
    ```

    ON MAINFRAMES
    - files must be in native EBCDIC encoding

    ON WINDOWS
    - plain data files can be in ASCII encoding
    - files with signed, comp, comp-3 fields must be in EBCDIC encoding (binary transfer)

- copy

    ```cobol
          01  field01.
               03 rec-header.
    01-01         05 rec-header-field-01     pic x(1).
    02-03         05 rec-header-field-02     pic x(2).
    04-06         05 rec-header-field-03     pic x(3).
    07-07      03 rec-type                   pic x(1).
                  88 rec-type-1                        value '1'.
                  88 rec-type-2                        value '2'.
                  88 rec-type-3                        value '3'.
    08-08      03 rec-subtype                pic x(1).
                  88 rec-subtype-0                     value '0'.
                  88 rec-subtype-1                     value '1'.
                  88 rec-subtype-2                     value '2'.
    09-14      03 rec-body                   pic x(6).
    09-14      03 rec-body-01
                  redefines rec-body.
                  05 rec-body-01-field-01    pic x(1).
                  05 rec-body-01-field-02    pic x(2).
                  05 rec-body-01-field-03    pic x(3).
    09-14      03 rec-body-02-01
                  redefines rec-body.
                  05 rec-body-02-01-field-02 pic x(2).
                  05 filler                  pic x(3).
                  05 rec-body-02-01-field-01 pic x(1).
    09-14      03 rec-body-02-02
                  redefines rec-body         pic x(6).
    09-14      03 rec-body-03
                  redefines rec-body.
                  05 rec-body-03-field-03    pic x(3).
                  05 rec-body-03-field-01    pic x(1).
                  05 filler                  pic x(2).
    15-15      03 rec-footer                 pic x(1).
    ```

At start, the tool shows this menu:

    blip> enter command:
        e: edit console
        c: view copy
        d: view data...
        x: exit blip

- **e** shows a file with the parms for the tool

    ```
    *
    * file data
    *     dataset
              C:\ ... blip\data\example1.ascii
    *     ebcdic/ascii
              ascii
    *     lseq/seq
              lseq
    *
    * file copy
    *     dataset
              C:\ ... blip\data\example1.cpy
    *
    * options
    *     max number of records
              99
    *     max length for alphanumeric fields
              99
    *     restart record cursor for level
              99
    *
    *
    *
    ```

    The dataset and the copy can be set here.
    
    - **lseq** means that every record in the dataset terminates with a newline char (**seq** means the opposite).
    
        The presence of the newline char depends on what type of file-transfer is used.

- **c** shows the copy as interpreted by the tool:

    ```
    ;level;label                               ;picture             ; occurs;redefines                           ;  int;  dec; from;   to;  len;structure                                                                                           ;
    ;     ;                                    ;                    ;       ;                                    ;     ;     ;     ;     ;     ;                                                                                                    ;
    ;    1;field01                             ;GROUP               ;       ;                                    ;    0;    0;    1;   15;   15; |01                                                                                                ;
    ;    3;rec-header                          ;GROUP               ;       ;                                    ;    0;    0;    1;    6;    6; | |03                                                                                              ;
    ;    5;rec-header-field-01                 ;x                   ;       ;                                    ;    1;    0;    1;    1;    1; | | |05                                                                                            ;
    ;    5;rec-header-field-02                 ;x(2)                ;       ;                                    ;    2;    0;    2;    3;    2; | | |05                                                                                            ;
    ;    5;rec-header-field-03                 ;x(3)                ;       ;                                    ;    3;    0;    4;    6;    3; | | |05                                                                                            ;
    ;    3;rec-type                            ;x                   ;       ;                                    ;    1;    0;    7;    7;    1; | |03                                                                                              ;
    ;    3;rec-subtype                         ;x                   ;       ;                                    ;    1;    0;    8;    8;    1; | |03                                                                                              ;
    ;    3;rec-body                            ;x(6)                ;       ;                                    ;    6;    0;    9;   14;    6; | |03                                                                                              ;
    ;    3;rec-body-01                         ;GROUP               ;       ;rec-body                            ;    0;    0;    9;   14;    6; | |03                                                                                              ;
    ;    5;rec-body-01-field-01                ;x                   ;       ;                                    ;    1;    0;    9;    9;    1; | | |05                                                                                            ;
    ;    5;rec-body-01-field-02                ;x(2)                ;       ;                                    ;    2;    0;   10;   11;    2; | | |05                                                                                            ;
    ;    5;rec-body-01-field-03                ;x(3)                ;       ;                                    ;    3;    0;   12;   14;    3; | | |05                                                                                            ;
    ;    3;rec-body-02-01                      ;GROUP               ;       ;rec-body                            ;    0;    0;    9;   14;    6; | |03                                                                                              ;
    ;    5;rec-body-02-01-field-02             ;x(2)                ;       ;                                    ;    2;    0;    9;   10;    2; | | |05                                                                                            ;
    ;    5;filler-(1)                          ;x(3)                ;       ;                                    ;    3;    0;   11;   13;    3; | | |05                                                                                            ;
    ;    5;rec-body-02-01-field-01             ;x                   ;       ;                                    ;    1;    0;   14;   14;    1; | | |05                                                                                            ;
    ;    3;rec-body-02-02                      ;x(6)                ;       ;rec-body                            ;    6;    0;    9;   14;    6; | |03                                                                                              ;
    ;    3;rec-body-03                         ;GROUP               ;       ;rec-body                            ;    0;    0;    9;   14;    6; | |03                                                                                              ;
    ;    5;rec-body-03-field-03                ;x(3)                ;       ;                                    ;    3;    0;    9;   11;    3; | | |05                                                                                            ;
    ;    5;rec-body-03-field-01                ;x                   ;       ;                                    ;    1;    0;   12;   12;    1; | | |05                                                                                            ;
    ;    5;filler-(2)                          ;x(2)                ;       ;                                    ;    2;    0;   13;   14;    2; | | |05                                                                                            ;
    ;    3;rec-footer                          ;x                   ;       ;                                    ;    1;    0;   15;   15;    1; | |03                                                                                              ;
    ```

 - **d** opens a submenu:
 
    ```
    blip> viewData> enter command:
    r: select records...
    c: select columns...
    v: view data
    m: return to parent menu
    x: exit blip
    ```

Here, **r** and **c** allow you to filter the rows and columns to be displayed, and to set the criteria for which a certain REDEFINE must be used for a given condition.

In the example, let's say we filter the rows as follows:

     field ------------------------------------- query
     ALL FIELDS                                  [][]
     field01.                                    [][]
      rec-header.                                [][]
       rec-header-field-01                       [][]
       rec-header-field-02                       [][]
       rec-header-field-03                       [][]
      rec-type                                   [>][1]
      rec-subtype                                [][]
      rec-body                                   [][]
      rec-body-01.                               [][]
       rec-body-01-field-01                      [][]
       rec-body-01-field-02                      [][]
       rec-body-01-field-03                      [][]
      rec-body-02-01.                            [][]
       rec-body-02-01-field-02                   [][]
       filler-(1)                                [][]
       rec-body-02-01-field-01                   [][]
      rec-body-02-02                             [][]
      rec-body-03.                               [][]
       rec-body-03-field-03                      [][]
       rec-body-03-field-01                      [][]
       filler-(2)                                [][]
      rec-footer                                 [][]

And let's say we define the columns as follows:

     when [rec-type][=][01]
     show [rec-header]
     and  [rec-type]
     and  [rec-body-01]
     and  [rec-footer]

     when [rec-type][=][02]
     and  [rec-subtype][=][01]
     show [rec-header]
     and  [rec-type]
     and  [rec-body-02-01]
     and  [rec-footer]

     when [rec-type][=][02]
     and  [rec-subtype][=][02]
     show [rec-header]
     and  [rec-type]
     and  [rec-body-02-02]
     and  [rec-footer]

     when [rec-type][=][03]
     show [rec-header]
     and  [rec-type]
     and  [rec-body-03]
     and  [rec-footer]

    default
    show [*]

With **v**, the tool provides a report as follows:

     rec    ; rec    ; rec    ; rec  ; rec   ; filler ; rec   ; rec    ; 
     header ; header ; header ; type ; body  ; (1)    ; body  ; footer ; 
     field  ; field  ; field  ;      ; 02    ;        ; 02    ;        ; 
     01     ; 02     ; 03     ;      ; 01    ;        ; 01    ;        ; 
            ;        ;        ;      ; field ;        ; field ;        ; 
            ;        ;        ;      ; 02    ;        ; 01    ;        ; 
            ;        ;        ;      ;       ;        ;       ;        ; 
     a      ; 03     ; xyz    ; 2    ; aa    ; @@@    ; b     ; .      ; 

     rec    ; rec    ; rec    ; rec  ; rec    ; rec    ; 
     header ; header ; header ; type ; body   ; footer ; 
     field  ; field  ; field  ;      ; 02     ;        ; 
     01     ; 02     ; 03     ;      ; 02     ;        ; 
            ;        ;        ;      ;        ;        ; 
            ;        ;        ;      ;        ;        ; 
            ;        ;        ;      ;        ;        ; 
     a      ; 04     ; xyz    ; 2    ; @@@@@@ ; .      ; 

     rec    ; rec    ; rec    ; rec  ; rec   ; rec   ; filler ; rec    ; 
     header ; header ; header ; type ; body  ; body  ; (2)    ; footer ; 
     field  ; field  ; field  ;      ; 03    ; 03    ;        ;        ; 
     01     ; 02     ; 03     ;      ; field ; field ;        ;        ; 
            ;        ;        ;      ; 03    ; 01    ;        ;        ; 
            ;        ;        ;      ;       ;       ;        ;        ; 
            ;        ;        ;      ;       ;       ;        ;        ; 
     a      ; 05     ; xyz    ; 3    ; aaa   ; b     ; @@     ; .      ; 

     rec    ; rec    ; rec    ; rec  ; rec   ; filler ; rec   ; rec    ; 
     header ; header ; header ; type ; body  ; (1)    ; body  ; footer ; 
     field  ; field  ; field  ;      ; 02    ;        ; 02    ;        ; 
     01     ; 02     ; 03     ;      ; 01    ;        ; 01    ;        ; 
            ;        ;        ;      ; field ;        ; field ;        ; 
            ;        ;        ;      ; 02    ;        ; 01    ;        ; 
            ;        ;        ;      ;       ;        ;       ;        ; 
     a      ; 07     ; xyz    ; 2    ; cc    ; @@@    ; d     ; .      ; 

     rec    ; rec    ; rec    ; rec  ; rec    ; rec    ; 
     header ; header ; header ; type ; body   ; footer ; 
     field  ; field  ; field  ;      ; 02     ;        ; 
     01     ; 02     ; 03     ;      ; 02     ;        ; 
            ;        ;        ;      ;        ;        ; 
            ;        ;        ;      ;        ;        ; 
            ;        ;        ;      ;        ;        ; 
     a      ; 08     ; xyz    ; 2    ; @@@@@@ ; .      ; 

     rec    ; rec    ; rec    ; rec  ; rec   ; rec   ; filler ; rec    ; 
     header ; header ; header ; type ; body  ; body  ; (2)    ; footer ; 
     field  ; field  ; field  ;      ; 03    ; 03    ;        ;        ; 
     01     ; 02     ; 03     ;      ; field ; field ;        ;        ; 
            ;        ;        ;      ; 03    ; 01    ;        ;        ; 
            ;        ;        ;      ;       ;       ;        ;        ; 
            ;        ;        ;      ;       ;       ;        ;        ; 
     a      ; 10     ; xyz    ; 3    ; ccc   ; d     ; @@     ; .      ; 
     a      ; 11     ; xyz    ; 3    ; eee   ; f     ; @@     ; .      ; 
     a      ; 12     ; xyz    ; 3    ; ggg   ; h     ; @@     ; .      ; 
     a      ; 13     ; xyz    ; 3    ; iii   ; j     ; @@     ; .      ; 

     rec    ; rec    ; rec    ; rec  ; rec     ; rec    ; rec   ; rec   ; rec   ; rec   ; filler ; rec   ; rec    ; rec   ; rec   ; filler ; rec    ; 
     header ; header ; header ; type ; subtype ; body   ; body  ; body  ; body  ; body  ; (1)    ; body  ; body   ; body  ; body  ; (2)    ; footer ; 
     field  ; field  ; field  ;      ;         ;        ; 01    ; 01    ; 01    ; 02    ;        ; 02    ; 02     ; 03    ; 03    ;        ;        ; 
     01     ; 02     ; 03     ;      ;         ;        ; field ; field ; field ; 01    ;        ; 01    ; 02     ; field ; field ;        ;        ; 
            ;        ;        ;      ;         ;        ; 01    ; 02    ; 03    ; field ;        ; field ;        ; 03    ; 01    ;        ;        ; 
            ;        ;        ;      ;         ;        ;       ;       ;       ; 02    ;        ; 01    ;        ;       ;       ;        ;        ; 
            ;        ;        ;      ;         ;        ;       ;       ;       ;       ;        ;       ;        ;       ;       ;        ;        ; 
     a      ; 14     ; xyz    ; 9    ; 0       ; abcdef ; a     ; bc    ; def   ; ab    ; cde    ; f     ; abcdef ; abc   ; d     ; ef     ; .      ; 

In this example, the last record doesn't match any criteria, so all fields of the copy are shown.

This repository is licensed under MIT (c) 2020 GitHub, Inc.





