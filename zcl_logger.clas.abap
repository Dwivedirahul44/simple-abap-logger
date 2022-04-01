CLASS zcl_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!

    METHODS add_log_s
      IMPORTING
                VALUE(is_msg)   TYPE bal_s_msg OPTIONAL
                VALUE(iv_msg)   TYPE string OPTIONAL
                  PREFERRED PARAMETER iv_msg
      RETURNING VALUE(rv_subrc) TYPE sysubrc.
    METHODS add_log_w
      IMPORTING
                VALUE(is_msg)   TYPE bal_s_msg OPTIONAL
                VALUE(iv_msg)   TYPE string OPTIONAL
                  PREFERRED PARAMETER iv_msg
      RETURNING VALUE(rv_subrc) TYPE sysubrc.
    METHODS add_log_e
      IMPORTING
                VALUE(is_msg)   TYPE bal_s_msg OPTIONAL
                VALUE(iv_msg)   TYPE string OPTIONAL
                  PREFERRED PARAMETER iv_msg
      RETURNING VALUE(rv_subrc) TYPE sysubrc    .
    METHODS constructor
      IMPORTING
                VALUE(iv_object)     TYPE balobj_d
                VALUE(iv_sub_object) TYPE balobj_d OPTIONAL
                VALUE(iv_extnum)     TYPE balnrext OPTIONAL .
  PROTECTED SECTION.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!

    DATA v_tr_log_hndl TYPE balloghndl .
    DATA v_tr_log_hndl_t TYPE bal_t_logh .

    METHODS ret_default_msg
      IMPORTING
        !iv_msg             TYPE char200
      RETURNING
        VALUE(rs_bal_s_msg) TYPE bal_s_msg .
    METHODS add_log
      IMPORTING
        VALUE(is_log_msg) TYPE bal_s_msg
      RETURNING
        VALUE(rv_subrc)   TYPE sysubrc  .
    METHODS save_log
      RETURNING
        VALUE(rv_subrc) TYPE sysubrc  .
ENDCLASS.



CLASS ZCL_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LOGGER->ADD_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_LOG_MSG                     TYPE        BAL_S_MSG
* | [<-()] RV_SUBRC                       TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_log.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = me->v_tr_log_hndl
        i_s_msg      = is_log_msg
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      rv_subrc = syst-subrc.
      RETURN.
    ENDIF.

    rv_subrc =  me->save_log( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_LOG_E
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG                         TYPE        BAL_S_MSG(optional)
* | [--->] IV_MSG                         TYPE        STRING(optional)
* | [<-()] RV_SUBRC                       TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_log_e.
    DATA:
      ls_msg TYPE bal_s_msg.

    DATA:
      lv_msg  TYPE char200.

    IF  is_msg IS INITIAL
    AND iv_msg IS INITIAL.
      rv_subrc = 4.
      RETURN.
    ENDIF.

    IF is_msg IS INITIAL.

      lv_msg = iv_msg.

      ls_msg = me->ret_default_msg( lv_msg ).

      ls_msg-msgty     = if_xo_const_message=>error.

    ELSE.

      ls_msg = is_msg.

    ENDIF.

    me->add_log( ls_msg ).

    rv_subrc = syst-subrc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_LOG_S
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG                         TYPE        BAL_S_MSG(optional)
* | [--->] IV_MSG                         TYPE        STRING(optional)
* | [<-()] RV_SUBRC                       TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_log_s.
************************************************************************
    DATA:
      ls_msg TYPE bal_s_msg.

    DATA:
      lv_msg  TYPE char200.

    IF  is_msg IS INITIAL
    AND iv_msg IS INITIAL.
      rv_subrc = 4.
      RETURN.
    ENDIF.

    IF is_msg IS INITIAL.

      lv_msg = iv_msg.

      ls_msg = me->ret_default_msg( lv_msg ).

      ls_msg-msgty     = if_xo_const_message=>success.

    ELSE.

      ls_msg = is_msg.

    ENDIF.

    me->add_log( ls_msg ).
    rv_subrc = syst-subrc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_LOG_W
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG                         TYPE        BAL_S_MSG(optional)
* | [--->] IV_MSG                         TYPE        STRING(optional)
* | [<-()] RV_SUBRC                       TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_log_w.
    DATA:
      ls_msg TYPE bal_s_msg.

    DATA:
      lv_msg  TYPE char200.

    IF  is_msg IS INITIAL
    AND iv_msg IS INITIAL.
      rv_subrc = 4.
      RETURN.
    ENDIF.

    IF is_msg IS INITIAL.

      lv_msg = iv_msg.

      ls_msg = me->ret_default_msg( lv_msg ).
      ls_msg-msgty     = if_xo_const_message=>warning.

    ELSE.

      ls_msg = is_msg.

    ENDIF.

    me->add_log( ls_msg ).

    rv_subrc = syst-subrc.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUB_OBJECT                  TYPE        BALOBJ_D(optional)
* | [--->] IV_EXTNUM                      TYPE        BALNREXT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    DATA:
      ls_log        TYPE bal_s_log.

* define some header data of this log
    ls_log-extnumber  = iv_extnum.
    ls_log-object     = iv_object.
    ls_log-subobject  = iv_sub_object.
    ls_log-aldate     = syst-datum.
    ls_log-altime     = syst-uzeit.
    ls_log-aluser     = syst-uname.
    ls_log-alprog     = syst-cprog.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = me->v_tr_log_hndl
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    " Sorted table, Insert is safer than append.
    INSERT me->v_tr_log_hndl INTO TABLE me->v_tr_log_hndl_t.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LOGGER->RET_DEFAULT_MSG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSG                         TYPE        CHAR200
* | [<-()] RS_BAL_S_MSG                   TYPE        BAL_S_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ret_default_msg.

* define data of message for Application Log.
    rs_bal_s_msg-msgid     = 'DB'.
    rs_bal_s_msg-msgno     = '000'.
    rs_bal_s_msg-msgv1     = iv_msg+0(50).
    rs_bal_s_msg-msgv2     = iv_msg+50(50).
    rs_bal_s_msg-msgv3     = iv_msg+100(50).
    rs_bal_s_msg-msgv4     = iv_msg+150(50).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LOGGER->SAVE_LOG
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SUBRC                       TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_log.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = me->v_tr_log_hndl_t
      EXCEPTIONS
        OTHERS         = 1.

    rv_subrc = syst-subrc.

  ENDMETHOD.
ENDCLASS.
