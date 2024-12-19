*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE create_regexp.
  " first try PCRE
  TRY.
      CALL METHOD cl_abap_regex=>('CREATE_PCRE')
        EXPORTING
          pattern = &2
        RECEIVING
          regex   = &1.                                     "#EC NOTEXT
    CATCH cx_sy_dyn_call_error.
      CREATE OBJECT &1
        EXPORTING
          pattern = &2 ##REGEX_POSIX ##NO_TEXT.             "#EC NOTEXT
  ENDTRY.
END-OF-DEFINITION.
