*"* use this source file for your ABAP unit test classes
class cl_test_issha_hash definition for testing RISK LEVEL HARMLESS.

public section.
methods: test_instantiation for testing,
         test_db_format for testing.
endclass.

CLASS cl_test_issha_hash IMPLEMENTATION.

  METHOD test_instantiation.
    data(issha_hash) = new zcl_issha_hash( password = 'test' iter = 1024 ).
  ENDMETHOD.

  METHOD test_db_format.
    data(issha_hash) = new zcl_issha_hash( password = 'test' iter = 1024 ).

    data(hash) = issha_hash->digest( ).

    data(expected) = |\{x-issha, 1024\}{ hash }|.

    cl_abap_unit_assert=>assert_equals( act = issha_hash->get_hash_in_db_format( ) exp = expected ).

  ENDMETHOD.

ENDCLASS.
