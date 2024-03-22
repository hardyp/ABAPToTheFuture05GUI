*&---------------------------------------------------------------------*
*& Report Z5_TDD_DEPENDENCIES_V03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
* In this program all calls to external systems are made by way of
* call to methods of classes
* You cannot subclass a function module, but you can subclass
* classes to simulate the actions of a user for example
* With all the dependencies broken, automated unit tests can be run
* upon the program
*--------------------------------------------------------------------*
REPORT z5_tdd_dependencies_v03.

*--------------------------------------------------------------------*
* Class Definitions / Implementations
*--------------------------------------------------------------------*
CLASS lcl_database_access DEFINITION.
  PUBLIC SECTION.
    METHODS read_customising.
ENDCLASS.                    "lcl_database_access DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_database_access IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_database_access IMPLEMENTATION.
  METHOD read_customising.
    CALL FUNCTION 'Z5READ_MONSTER_CUSTOMIZING'.
  ENDMETHOD.                    "read_customising
ENDCLASS.                    "lcl_database_access IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_missile_control DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_missile_control DEFINITION.
  PUBLIC SECTION.
    METHODS: read_nuclear_missile_sensor,
      tell_proxy_to_fire_missile.
ENDCLASS.                    "lcl_missile_control DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_missile_control IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_missile_control IMPLEMENTATION.
  METHOD read_nuclear_missile_sensor.
    CALL FUNCTION 'Z5READ_NUCLEAR_MISSILE_SENSOR'.
  ENDMETHOD.                    "read_nuclear_missile_sensor

  METHOD tell_proxy_to_fire_missile.
    CALL FUNCTION 'Z5TELL_PROXY_2_FIRE_MISSILE'.
  ENDMETHOD.                    "tell_proxy_to_fire_missile
ENDCLASS.                    "lcl_missile_control IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_user_interface DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_user_interface DEFINITION.
  PUBLIC SECTION.
    METHODS popup_to_confirm RETURNING VALUE(user_answer) TYPE char01.
ENDCLASS.                    "lcl_user_interface DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_user_interface IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_user_interface IMPLEMENTATION.
  METHOD popup_to_confirm.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Missile Confirmation'(001)
        text_question  = 'Do you want to launch the missile?'(002)
        text_button_1  = 'Yes'
        text_button_2  = 'No'
        default_button = '2'
      IMPORTING
        answer         = user_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "popup_to_confirm
ENDCLASS.                    "lcl_user_interface IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_printer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_printer DEFINITION.
  PUBLIC SECTION.
    METHODS print_nuclear_smartform.
ENDCLASS.                    "lcl_printer DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_printer IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_printer IMPLEMENTATION.
  METHOD print_nuclear_smartform.
    CALL FUNCTION 'Z5PRINT_NUCLEAR_SMARTFORM'.
  ENDMETHOD.                    "print_nuclear_smartform
ENDCLASS.                    "lcl_printer IMPLEMENTATION

*--------------------------------------------------------------------*
* Evil Global Variables
*--------------------------------------------------------------------*
DATA: go_database_access TYPE REF TO lcl_database_access,
      go_missile_control TYPE REF TO lcl_missile_control,
      go_user_interface  TYPE REF TO lcl_user_interface,
      go_printer         TYPE REF TO lcl_printer.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  go_database_access = NEW #( ).
  go_missile_control = NEW #( ).
  go_user_interface  = NEW #( ).
  go_printer         = NEW #( ).
  PERFORM fire_nuclear_missile.

*--------------------------------------------------------------------*
* Sub-Routines
*--------------------------------------------------------------------*
* Listing 04.03 : Calling Methods of Helper Classes
*--------------------------------------------------------------------*
FORM fire_nuclear_missile.

* Read Database
* Here the dependency is needing an actual database with real data
  go_database_access->read_customising( ).

* Query Missile Sensor
* Here the dependency is needing contact with an actual missile system
  go_missile_control->read_nuclear_missile_sensor( ).

* Actual Business Logic (that you want to test)
* You would want to test that the missile gets sent to the right place
* i.e. gets dropped on your enemy, not on you
* IF something.
*   "We aim the missile here
* ELSEIF something_else.
*   "We aim the missile here
* ENDIF.

* Ask the user if they want to fire the missile
* Here the dependency is on having an actual user
  DATA(user_answer) = go_user_interface->popup_to_confirm( ).

* Some more business logic
* You would want to test that saying "no" prevented the
* missile from firing
  CASE user_answer.
    WHEN '1'.
      "Off We Go! Bombs Away!
    WHEN '2'.
      RETURN.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

* Fire Missile
* Here the dependency is needing an actual missile to fire
  go_missile_control->tell_proxy_to_fire_missile( ).

* Print Results
* Here the dependency is needing an actual printer
  go_printer->print_nuclear_smartform( ).

*---------------------------------------------------------------------------------------------------------*
* As it stands the program is functionally totally unchanged
* The difference is, that in the main FORM there is only business
* logic. All external dependencies are handled by classes which can
* be subclassed
* Thus automated unit tests are now possible on this program
* I have deliberately left this as a procedural program, to show how
* you can change parts of your monolithic ten thousand line existing
* procedural program, to bring islands of code (individual routines)
* under test, a bit at a time
* There is an SAP PRESS EBITE about this called "Refactoring Legacy Code"
* https://www.sap-press.com/refactoring-legacy-abap-code_5234/
*----------------------------------------------------------------------------------------------------------*
ENDFORM.                    "fire_nuclear_missile
