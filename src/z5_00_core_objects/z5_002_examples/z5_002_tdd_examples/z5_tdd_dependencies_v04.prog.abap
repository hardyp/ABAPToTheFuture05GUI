*&---------------------------------------------------------------------*
*& Report Z5_TDD_DEPENDENCIES_V04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
* This time the program becomes 100% OO as far as is possible for an
* executable program
*--------------------------------------------------------------------*
* Now we declare interfaces for all the dependency classes, and
* create definitions and implementations for test double classes of
* each such class
*--------------------------------------------------------------------*
* But this version of the program has a fatal flaw, so automated
* unit testing is IMPOSSIBLE!
*--------------------------------------------------------------------*
REPORT z5_tdd_dependencies_v04.
*--------------------------------------------------------------------*
* Class Definitions / Implementations
*--------------------------------------------------------------------*
* Listing 04.05 : Coding Implementation for Test Doubles
*--------------------------------------------------------------------*
INTERFACE lif_database_access.
  METHODS read_customising.
ENDINTERFACE.

CLASS lcl_database_access DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_database_access.

    ALIASES: read_customising FOR lif_database_access~read_customising.
ENDCLASS.

CLASS lcl_database_access IMPLEMENTATION.
  METHOD read_customising.
    CALL FUNCTION 'Z5READ_MONSTER_CUSTOMIZING'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_database_access DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_database_access.

    ALIASES: read_customising FOR lif_database_access~read_customising.
ENDCLASS.

CLASS ltd_database_access IMPLEMENTATION.
  METHOD read_customising.
    "Return Bogus Database Values
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* Remote Connection to Nuclear Missile (via middleware such as SIS)
*----------------------------------------------------------------------*
INTERFACE lif_missile_control.
  METHODS: read_nuclear_missile_sensor,
    tell_proxy_to_fire_missile.
ENDINTERFACE.

CLASS lcl_missile_control DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_missile_control.

    ALIASES: read_nuclear_missile_sensor FOR lif_missile_control~read_nuclear_missile_sensor,
             tell_proxy_to_fire_missile  FOR lif_missile_control~tell_proxy_to_fire_missile.
ENDCLASS.                    "lcl_missile_control DEFINITION

CLASS lcl_missile_control IMPLEMENTATION.
  METHOD read_nuclear_missile_sensor.
    CALL FUNCTION 'Z5READ_NUCLEAR_MISSILE_SENSOR'.
  ENDMETHOD.                    "read_nuclear_missile_sensor

  METHOD tell_proxy_to_fire_missile.
    CALL FUNCTION 'Z5TELL_proxy_2_FIRE_MISSILE'.
  ENDMETHOD.                    "tell_proxy_to_fire_missile
ENDCLASS.                    "lcl_missile_control IMPLEMENTATION

CLASS ltd_missile_control DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_missile_control.

    ALIASES: read_nuclear_missile_sensor FOR lif_missile_control~read_nuclear_missile_sensor,
             tell_proxy_to_fire_missile  FOR lif_missile_control~tell_proxy_to_fire_missile.
ENDCLASS.

CLASS ltd_missile_control IMPLEMENTATION.
  METHOD read_nuclear_missile_sensor.
    "Return Bogus Value
  ENDMETHOD.                    "read_nuclear_missile_sensor

  METHOD tell_proxy_to_fire_missile.
    "Do Nothing
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* User Interface
*----------------------------------------------------------------------*
INTERFACE lif_user_interface.
  METHODS popup_to_confirm RETURNING VALUE(user_answer) TYPE char01.
ENDINTERFACE.

CLASS lcl_user_interface DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_user_interface.

    ALIASES: popup_to_confirm FOR lif_user_interface~popup_to_confirm.
ENDCLASS.                    "lcl_user_interface DEFINITION

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

CLASS ltd_user_interface DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_user_interface.

    ALIASES: popup_to_confirm FOR lif_user_interface~popup_to_confirm.
ENDCLASS.

CLASS ltd_user_interface IMPLEMENTATION.
  METHOD popup_to_confirm.
    user_answer = '1'.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* Printer
*----------------------------------------------------------------------*
INTERFACE lif_printer.
  METHODS print_nuclear_smartform.
ENDINTERFACE.

CLASS lcl_printer DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_printer.

    ALIASES: print_nuclear_smartform FOR lif_printer~print_nuclear_smartform.
ENDCLASS.                    "lcl_printer DEFINITION

CLASS lcl_printer IMPLEMENTATION.
  METHOD print_nuclear_smartform.
    CALL FUNCTION 'Z5PRINT_NUCLEAR_SMARTFORM'.
  ENDMETHOD.                    "print_nuclear_smartform
ENDCLASS.                    "lcl_printer IMPLEMENTATION

CLASS ltd_printer DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_printer.

    ALIASES: print_nuclear_smartform FOR lif_printer~print_nuclear_smartform.
ENDCLASS.

CLASS ltd_printer IMPLEMENTATION.
  METHOD print_nuclear_smartform.
    "Do Nothing
  ENDMETHOD.
ENDCLASS.
*---------------------------------------------------------------------------*
* Main Application
*---------------------------------------------------------------------------*
* Listing 04:07 : Helper Classes as Private Instance Variables of Main Class
*---------------------------------------------------------------------------*
CLASS lcl_missile_firer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             fire_nuclear_missile.

  PRIVATE SECTION.
    DATA: mo_database_access TYPE REF TO lif_database_access,
          mo_missile_control TYPE REF TO lif_missile_control,
          mo_user_interface  TYPE REF TO lif_user_interface,
          mo_printer         TYPE REF TO lif_printer.
ENDCLASS.

CLASS lcl_missile_firer IMPLEMENTATION.
*------------------------------------------------------------------------*
* Listing 04:08 : Variables Set Up during Construction of Object Instance
*------------------------------------------------------------------------*
  METHOD constructor.

      mo_database_access = NEW lcl_database_access( ).
      mo_missile_control = NEW lcl_missile_control( ).
      mo_user_interface  = NEW lcl_user_interface( ).
      mo_printer         = NEW lcl_printer( ).

  ENDMETHOD.

  METHOD fire_nuclear_missile.
* Read Database
* Here the dependency is needing an actual database with real data
    mo_database_access->read_customising( ).

* Query Missile Sensor
* Here the dependency is needing contact with an actual missile system
    mo_missile_control->read_nuclear_missile_sensor( ).

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
    DATA(user_answer) = mo_user_interface->popup_to_confirm( ).

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
    mo_missile_control->tell_proxy_to_fire_missile( ).

* Print Results
* Here the dependency is needing an actual printer
    mo_printer->print_nuclear_smartform( ).

  ENDMETHOD.

ENDCLASS."Main Application Class Definition
*--------------------------------------------------------------------*
* Now - the traditional part of the program!
*--------------------------------------------------------------------*
* Listing 04.06 : Call Method instead of FORM Routine
*--------------------------------------------------------------------*
START-OF-SELECTION.
  NEW lcl_missile_firer( )->fire_nuclear_missile( ).
