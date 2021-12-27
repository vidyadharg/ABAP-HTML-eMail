# SAP ABAP send email using eMail Templates with or without CDS
##  1.	Create E-Mail Template
  ![image](https://user-images.githubusercontent.com/28149363/147467514-c82fdba0-99b3-40f8-b760-3dd5b9f33173.png)

  ![image](https://user-images.githubusercontent.com/28149363/147462388-06d0f6af-719b-4743-a4c4-69bdbc3ae2a2.png)

  ![image](https://user-images.githubusercontent.com/28149363/147462415-02dc8b41-8c63-44ba-a7f1-70e0a8f6e6fb.png)


##  2.	Create class ZCL_EMAIL inheriting from CL_BCS_MESSAGE  

    2.1.	METHODS SET_BODY_SO10 " get Email body from so10 text.

    2.2.	METHODS SET_PLACEHOLDER

    2.3.	METHODS replace_placeholder " replace placehoder for non cds email template

    2.4.	METHODS SET_SUB_BODY_TEMPLATE "read email body from email template

      "2.4.1.	Dont pass place holders when Email teplate created without CDS.

      SELECT SINGLE cds_view FROM smtg_tmpl_hdr
              INTO @DATA(lv_cds_view)
              WHERE id      EQ @iv_template_id
                AND version EQ 'A'. "GC_VERSION_ACTIVE
            IF sy-subrc EQ 0.
              IF lv_cds_view IS NOT INITIAL.
                DATA(lt_data_key) = gt_data_key.
              ENDIF.

      "2.4.2.	Read email template in HTML or Text.
        TRY.
            DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = iv_template_id ).

            lo_email_api->render(
              EXPORTING
                iv_language  = iv_lang
                it_data_key  = lt_data_key
              IMPORTING
                ev_subject   = DATA(lv_subject)
                ev_body_html = DATA(lv_body_html)
                ev_body_text = DATA(lv_body_text) ).

          CATCH cx_smtg_email_common INTO DATA(ex). " E-Mail API Exceptions
        ENDTRY. 

      "2.4.3.	Replace placehoder for non-cds email template

          IF iv_doctype EQ 'HTM'.
            DATA(lv_mailbody) = lv_body_html.
          ELSE.
            lv_mailbody = lv_body_text.
          ENDIF.

          IF lv_cds_view IS INITIAL.
            lv_mailbody = replace_placeholder( lv_mailbody ).
            lv_subject = replace_placeholder( lv_subject ).
          ENDIF.

      "2.4.4.	set subject and body using supper class CL_BCS_MESSAGE 

         set_subject( lv_subject ).
         
         set_main_doc(
                 EXPORTING
                   iv_contents_txt = lv_mailbody      " Main Documet, First Body Part
                   iv_doctype      = iv_doctype ).     " Document Category

    ```

##  3.	Demo program  

```
REPORT zdemo_email.
"send email with HTML body using email template

TRY.
    DATA(email) = NEW zcl_email( ).
    data lv_expiry_dt TYPE dats.

    lv_expiry_dt  = sy-datum + 10. "add logic to get user id expiry date

    DATA(lv_days) = lv_expiry_dt - sy-datum.

    email->add_recipient( iv_address = CONV #( 'recipient@emailid.com' ) ).

    email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply ' ).

    email->set_placeholder(
      EXPORTING
        placeholder_name  = '&USER_FIRST_NAME&'
        placeholder_value =  CONV #( 'User firstname' ) ).

    email->set_placeholder(
      EXPORTING
        placeholder_name  = '&USER_ID&'
        placeholder_value = CONV #( 'user_id' ) ).


    email->set_placeholder(
      EXPORTING
        placeholder_name  =  '&EXPIRY_DT&'
        placeholder_value =  |{ lv_expiry_dt DATE = ISO }| ).

    email->set_placeholder(
      EXPORTING
        placeholder_name  =  '&DAYS&'
        placeholder_value =  CONV #( lv_days ) ).

    email->set_subject_body_template(
      EXPORTING
        template_id = 'ZET_DEMO' "Email body from Email template
        doctype      = 'HTM' ).

    email->set_send_immediately( abap_false ).
    email->send( ).

  CATCH cx_bcs_send INTO DATA(ex).
    MESSAGE ex->get_text( ) TYPE 'S'.
ENDTRY.
```

##  4.	SOST email preview
![image](https://user-images.githubusercontent.com/28149363/147467306-71169842-3552-40bd-a167-f65675e1c5bd.png)

