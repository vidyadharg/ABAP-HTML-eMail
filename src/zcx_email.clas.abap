CLASS zcx_email DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    DATA msgv1 TYPE symsgv READ-ONLY .
    DATA msgv2 TYPE symsgv READ-ONLY .
    DATA msgv3 TYPE symsgv READ-ONLY .
    DATA msgv4 TYPE symsgv READ-ONLY .
    DATA mv_longtext TYPE string READ-ONLY .

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        msgv1    TYPE symsgv OPTIONAL
        msgv2    TYPE symsgv OPTIONAL
        msgv3    TYPE symsgv OPTIONAL
        msgv4    TYPE symsgv OPTIONAL
        longtext TYPE csequence OPTIONAL .
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
        ix_previous     TYPE REF TO cx_root OPTIONAL
        iv_longtext     TYPE csequence OPTIONAL
      RAISING
        zcx_email .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_email IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    mv_longtext = longtext.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.


  METHOD raise_t100.
    DATA: ls_t100_key TYPE scx_t100key.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.

    IF iv_msgid IS INITIAL.
      CLEAR ls_t100_key.
    ENDIF.

    RAISE EXCEPTION NEW zcx_email( textid   = ls_t100_key
                                   msgv1    = iv_msgv1
                                   msgv2    = iv_msgv2
                                   msgv3    = iv_msgv3
                                   msgv4    = iv_msgv4
                                   previous = ix_previous
                                   longtext = iv_longtext ).
  ENDMETHOD.
ENDCLASS.
