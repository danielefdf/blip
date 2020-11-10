# blip
A command-line tool for visualizing fixed format files from mainframe

On the mainframe it may happen to come across fixed format files, i.e. files in which data is written in this way:

    01NAME1      SURNAME101/01/0001
    02NAME2      SURNAME202/01/0001
    03NAME3      SURNAME303/01/0001

Everything is simple until you find files in which the records are defined differently (see CoBOL REDEFINE) according to one or more reference fields: in this case it is not easy to read the file, especially if there are many redefinitions and many records.

This tool allows you to interpret the data, given the reference CoBOL structure.

## use of the tool

For example, lets' define the data and the reference CoBOL copy

- data

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

- copy

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

At start, the tool shows this menu:

    blip> enter command:
        e: edit console
        c: view copy
        d: view data...
        x: exit blip

- **e** just shows a file with some options

- **c** shows the copy

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


This repository is licensed under MIT (c) 2020 GitHub, Inc.
