CLASS zcl_email_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_cc TYPE bcs_copy VALUE 'C'.                "#EC NOTEXT
    CONSTANTS gc_bcc TYPE bcs_copy VALUE 'B'.               "#EC NOTEXT

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">validate email id</p>
      is_emailid_valid
        IMPORTING
          emailid                 TYPE ad_smtpadr
        RETURNING
          VALUE(is_emailid_valid) TYPE abap_bool,

      is_dl_shared
        IMPORTING dl_name            TYPE so_dli_nam
        RETURNING VALUE(r_shared_dl) TYPE so_text001,

      file_get_extension
        IMPORTING filename         TYPE string
                  uppercase        TYPE c DEFAULT 'X'
        RETURNING VALUE(extension) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_email_utility IMPLEMENTATION.
  METHOD is_dl_shared.

    SELECT SINGLE owntp, ownyr, ownno
    FROM soid INTO @DATA(ls_soid)
    WHERE objnam = @dl_name AND
          dlitp EQ 'DLI'.

    IF ls_soid IS INITIAL.
      "shared DL
      r_shared_dl = 'X'.
    ELSE.
      "Private DL
      r_shared_dl = space.
    ENDIF.
  ENDMETHOD.

  METHOD file_get_extension.

    DATA: strs TYPE STANDARD TABLE OF string,
          str  TYPE string,
          cnt  TYPE i.

* check for trailing dot
    cnt = strlen( filename ) - 1.
    IF cnt < 1 OR filename+cnt(1) = '.'.
      extension = ctsc_file_default_extension.
      RETURN.
    ENDIF.
* split into strings separated by dots,
* but the last could be empty unrecognized
    SPLIT filename AT '.' INTO TABLE strs.

    DESCRIBE TABLE strs LINES cnt.
    READ TABLE strs INTO str INDEX cnt.
    extension = str.

* translate to upper case if desired
    IF uppercase = 'X'.
      TRANSLATE extension TO UPPER CASE.
    ENDIF.

    IF extension = space OR cnt < 2.
      extension = 'BIN'.
    ENDIF.

  ENDMETHOD.


  METHOD is_emailid_valid.

    CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
      EXPORTING
        address_unstruct    = VALUE sx_address( type = 'INT' address = emailid )
      EXCEPTIONS
        error_address_type  = 1
        error_address       = 2
        error_group_address = 3
        OTHERS              = 4.
    IF sy-subrc EQ 0.
      is_emailid_valid = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
