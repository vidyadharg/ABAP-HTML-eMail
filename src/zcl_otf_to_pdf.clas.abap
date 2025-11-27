"! <p class="shorttext synchronized" lang="en">Converts the OTF into PDF</p>
CLASS zcl_otf_to_pdf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      bin_filesize TYPE i READ-ONLY.

    METHODS constructor .

    METHODS convert
      IMPORTING
        otfdata                TYPE tsfotf
      RETURNING
        VALUE(pdf_file_xstring) TYPE xstring
      RAISING
        zcx_email .
  PROTECTED SECTION.

private section.
ENDCLASS.



CLASS ZCL_OTF_TO_PDF IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD convert.
    DATA:
      lt_dummy    TYPE TABLE OF tline,
      lt_soli_tab TYPE soli_tab.

    IF otfdata IS INITIAL.

      RAISE EXCEPTION TYPE zcx_email
        EXPORTING
          textid = zcx_email=>otfdata_is_empty.

    ELSE.
      APPEND LINES OF otfdata TO lt_soli_tab.

      "convert otf to pdf format
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format       = 'PDF'
        IMPORTING
          bin_filesize = bin_filesize
          bin_file     = pdf_file_xstring
        TABLES
          otf          = lt_soli_tab
          lines        = lt_dummy
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc <> 0.
        "Error converting OTF to PDF
        zcx_email=>raise_excep( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
