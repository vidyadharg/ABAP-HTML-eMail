# SAP ABAP send email using eMail Templates with or without CDS
##  1.	Create E-Mail Template
  ![image](https://user-images.githubusercontent.com/28149363/147467514-c82fdba0-99b3-40f8-b760-3dd5b9f33173.png)

![image](https://user-images.githubusercontent.com/28149363/171146686-18c41e2f-1d1a-466d-be1a-0b2e818f911f.png)

![image](https://user-images.githubusercontent.com/28149363/171146887-5a05bbf1-44ca-4f1c-8a2b-9daeb7f3043a.png)


##  2.	Create class ZCL_EMAIL inheriting from CL_BCS_MESSAGE  

    2.1.	METHODS SET_BODY_SO10 " get Email body from so10 text.

    2.2.	METHODS SET_PLACEHOLDER " set dynamic variable to be replaced in email to body

    2.3.	METHODS replace_placeholder " replace placehoder for non cds email template
    
    2.4.	METHODS SET_PLACEHOLDER_ITAB " replace Itab  placehoder for non cds email template

    2.5.	METHODS SET_SUB_BODY_TEMPLATE "read email body from email template

      "2.5.1.	Dont pass place holders when Email teplate created without CDS.

      SELECT SINGLE cds_view FROM smtg_tmpl_hdr
              INTO @DATA(lv_cds_view)
              WHERE id      EQ @iv_template_id
                AND version EQ 'A'. "GC_VERSION_ACTIVE
            IF sy-subrc EQ 0.
              IF lv_cds_view IS NOT INITIAL.
                DATA(lt_data_key) = gt_data_key.
              ENDIF.

      "2.5.2.	Read email template in HTML or Text.
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

      "2.5.3.	Replace placehoder for non-cds email template

          IF iv_doctype EQ 'HTM'.
            DATA(lv_mailbody) = lv_body_html.
          ELSE.
            lv_mailbody = lv_body_text.
          ENDIF.

          IF lv_cds_view IS INITIAL.
            lv_mailbody = replace_placeholder( lv_mailbody ).
            lv_subject = replace_placeholder( lv_subject ).
          ENDIF.

      "2.5.4.	set subject and body using supper class CL_BCS_MESSAGE 

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

  select CARRID, CONNID, FLDATE, PRICE
    UP TO 10 ROWS FROM SFLIGHT INTO TABLE @data(lt_SFLIGHT).

    lv_expiry_dt  = sy-datum + 10. "add logic to get user id expiry date

    DATA(lv_days) = lv_expiry_dt - sy-datum.

    email->add_recipient( iv_address = CONV #( 'recipient@emailid.com' ) ).

    email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply ' ).

    "set internal table as html email body 
    email->set_placeholder_itab(
      EXPORTING
        placeholder_name  = '&SFLIGHT_TAB&'
        placeholder_itab = lt_SFLIGHT
        table_Title =  'Flights Details').

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


**
    "can pass multiple placeholders in itab
         email->set_placeholder(
            EXPORTING
              placeholder_tab  = VALUE if_smtg_email_template=>ty_gt_data_key(
               ( name = '&USER_FIRST_NAME&' value = 'User firstname' )
               ( name = '&USER_ID&'         value = 'user_id' )
               ( name = '&DAYS&'            value = lv_days )
               ( name = '&EXPIRY_DT&'       value = |{ lv_expiry_dt DATE = ISO }| )
                ) ).
**

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
![image](https://user-images.githubusercontent.com/28149363/171146985-1208eb8c-8204-4c30-875a-bbf3b5309ae9.png)


