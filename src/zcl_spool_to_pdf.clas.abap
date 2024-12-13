"! <p class="shorttext synchronized" lang="en">Converts the Spool into PDF</p>
CLASS zcl_spool_to_pdf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS convert
      IMPORTING spoolid          TYPE rspoid
      RETURNING VALUE(pdf_file_xstring) TYPE xstring
                "pdf_file_size    TYPE i
      RAISING   zcx_email.

  PROTECTED SECTION.

ENDCLASS.



CLASS zcl_spool_to_pdf IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD convert.
    DATA rq       TYPE tsp01.
    DATA bin_size TYPE i.
    DATA dummy    TYPE TABLE OF rspoattr.

*   ------------ get attributes of spool request ---------------------
    CALL FUNCTION 'RSPO_GET_ATTRIBUTES_SPOOLJOB'
      EXPORTING
        rqident     = spoolid
      IMPORTING
        rq          = rq
      TABLES
        attributes  = dummy
      EXCEPTIONS
        no_such_job = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      zcx_email=>raise_t100( EXPORTING iv_msgid    = 'PO'
                                       iv_msgno    = 126
                                       iv_msgv1    = CONV #( spoolid ) ).
    ENDIF.

*   --- convert spool request into PDF, dependent on document type ---
    IF rq-rqdoctype = 'OTF' OR rq-rqdoctype = 'SMART'.
      CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = spoolid
          no_dialog                = 'X'
          pdf_destination          = 'X'
          no_background            = 'X'
        IMPORTING
          "pdf_bytecount            = pdf_file_size
          bin_file                 = pdf_file_xstring
        EXCEPTIONS
          err_no_otf_spooljob      = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_dstdevice        = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
        zcx_email=>raise_t100( EXPORTING iv_msgid    = 'PO'
                                         iv_msgno    = 712
                                         iv_msgv1    = CONV #( sy-subrc )
                                         iv_msgv2    = CONV #( 'CONVERT_OTFSPOOLJOB_2_PDF' ) ).
      ENDIF.
    ELSEIF rq-rqdoctype = 'LIST'.
      CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = spoolid
          no_dialog                = 'X'
          pdf_destination          = 'X'
          no_background            = 'X'
        IMPORTING
          "pdf_bytecount            = pdf_file_size
          bin_file                 = pdf_file_xstring
        EXCEPTIONS
          err_no_abap_spooljob     = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_destdevice       = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
        zcx_email=>raise_t100( EXPORTING iv_msgid    = 'PO'
                                         iv_msgno    = 712
                                         iv_msgv1    = CONV #( sy-subrc )
                                         iv_msgv2    = CONV #( 'CONVERT_OTFSPOOLJOB_2_PDF' ) ).
      ENDIF.
    ELSE.
        zcx_email=>raise_t100( EXPORTING iv_msgid    = 'PO'
                                         iv_msgno    = 789
                                         iv_msgv1    = CONV #( rq-rqdoctype ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
