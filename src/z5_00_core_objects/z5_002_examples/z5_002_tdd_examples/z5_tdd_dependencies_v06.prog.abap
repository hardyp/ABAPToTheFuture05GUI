*&---------------------------------------------------------------------*
*& Report Z5_TDD_DEPENDENCIES_V06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
* This time instead of constructor injection to make the
* program testable we are going to use DEPENDENCY LOOKUP as
* proposed in an Open SAP course about TDD
*--------------------------------------------------------------------*
REPORT z5_tdd_dependencies_v06.
*--------------------------------------------------------------------*
* Class Definitions / Implementations
*--------------------------------------------------------------------*
CLASS lcl_monster_factory  DEFINITION DEFERRED.
CLASS lcl_monster_injector DEFINITION DEFERRED.
*--------------------------------------------------------------------*
* Listing 04.05 : Coding Implementation for Test Doubles
*--------------------------------------------------------------------*
INTERFACE lif_database_access.
  METHODS read_customising.
ENDINTERFACE.

*--------------------------------------------------------------------*
* Listing 4.12 : Revised Database Access Class Definition
*--------------------------------------------------------------------*
CLASS lcl_database_access DEFINITION
  CREATE PRIVATE
  FRIENDS lcl_monster_factory.

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

CLASS lcl_missile_control DEFINITION
  CREATE PRIVATE
  FRIENDS lcl_monster_factory.
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
    CALL FUNCTION 'Z5TELL_PROXY_2_FIRE_MISSILE'.
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

CLASS lcl_user_interface DEFINITION
  CREATE PRIVATE
  FRIENDS lcl_monster_factory.
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

CLASS lcl_printer DEFINITION
  CREATE PRIVATE
  FRIENDS lcl_monster_factory.
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
*--------------------------------------------------------------------*
* Listing 04:11 : Factory to Create Helper Objects
*--------------------------------------------------------------------*
CLASS lcl_monster_factory DEFINITION
  CREATE PRIVATE
  FRIENDS lcl_monster_injector.

  PUBLIC SECTION.

    METHODS get_database_access
      RETURNING
        VALUE(ro_database_access)
          TYPE REF TO lif_database_access.
    METHODS get_missile_control
      RETURNING
        VALUE(ro_missile_control)
          TYPE REF TO lif_missile_control.
    METHODS get_user_interface
      RETURNING
        VALUE(ro_user_interface)
          TYPE REF TO lif_user_interface.
    METHODS get_printer
      RETURNING
        VALUE(ro_printer)
          TYPE REF TO lif_printer.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_factory) TYPE REF TO lcl_monster_factory .

  PRIVATE SECTION.

    DATA mo_database_access
    TYPE REF TO lif_database_access.
    DATA mo_missile_control
    TYPE REF TO lif_missile_control.
    DATA mo_user_interface
    TYPE REF TO lif_user_interface.
    DATA: mo_printer
    TYPE REF TO lif_printer.

    CLASS-DATA mo_factory TYPE REF TO lcl_monster_factory.

ENDCLASS.
*--------------------------------------------------------------------*
* Listing 04:14 Singleton Pattern
*--------------------------------------------------------------------*
CLASS lcl_monster_factory IMPLEMENTATION.

  METHOD get_instance.

    IF mo_factory IS NOT BOUND.
      mo_factory = NEW #( ).
    ENDIF.

    ro_factory = mo_factory.

  ENDMETHOD.

  METHOD get_database_access.

    IF mo_database_access IS NOT BOUND.
      mo_database_access = NEW lcl_database_access( ).
    ENDIF.

    ro_database_access = mo_database_access.

  ENDMETHOD.

  METHOD get_missile_control.

    IF mo_missile_control IS NOT BOUND.
      mo_missile_control = NEW lcl_missile_control( ).
    ENDIF.

    ro_missile_control = mo_missile_control.

  ENDMETHOD.

  METHOD get_user_interface.

    IF mo_user_interface IS NOT BOUND.
      mo_user_interface = NEW lcl_user_interface( ).
    ENDIF.

    ro_user_interface = mo_user_interface.

  ENDMETHOD.

  METHOD get_printer.

    IF mo_printer IS NOT BOUND.
      mo_printer = NEW lcl_printer( ).
    ENDIF.

    ro_printer = mo_printer.

  ENDMETHOD.

ENDCLASS.
*--------------------------------------------------------------------*
* Monster Injector
*--------------------------------------------------------------------*
* Listing 04.15 : Definition of Monster Injector Class
*--------------------------------------------------------------------*
CLASS lcl_monster_injector DEFINITION
  FINAL
  CREATE PUBLIC
  FOR TESTING.

  PUBLIC SECTION.
    METHODS: constructor,
      inject_database_access
        IMPORTING io_database_access TYPE REF TO lif_database_access,
      inject_missile_control
        IMPORTING io_missile_control TYPE REF TO lif_missile_control,
      inject_user_interface
        IMPORTING io_user_interface TYPE REF TO lif_user_interface,
      inject_printer
        IMPORTING io_printer TYPE REF TO lif_printer.

  PRIVATE SECTION.
    DATA: mo_factory TYPE REF TO lcl_monster_factory.

ENDCLASS.

CLASS lcl_monster_injector IMPLEMENTATION.

  METHOD constructor.

    CLEAR lcl_monster_factory=>mo_factory.

    mo_factory = lcl_monster_factory=>get_instance( ).

  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 04.16: Injection Method
*--------------------------------------------------------------------*
  METHOD inject_database_access.

    mo_factory->mo_database_access = io_database_access.

  ENDMETHOD.

  METHOD inject_missile_control.

    mo_factory->mo_missile_control = io_missile_control.

  ENDMETHOD.

  METHOD inject_user_interface.

    mo_factory->mo_user_interface = io_user_interface.

  ENDMETHOD.

  METHOD inject_printer.

    mo_factory->mo_printer = io_printer.

  ENDMETHOD.

ENDCLASS.
*--------------------------------------------------------------------*
* Main Application
*--------------------------------------------------------------------*
* Listing 04.09 : Redesigned Constructor
*--------------------------------------------------------------------*
* Listing 04.15 : Enable Testing of Private Methods
*--------------------------------------------------------------------*
CLASS ltc_missile_firer DEFINITION DEFERRED.

CLASS lcl_missile_firer DEFINITION FRIENDS ltc_missile_firer.
  PUBLIC SECTION.
    METHODS:
      constructor,
      fire_nuclear_missile.

  PRIVATE SECTION.
    DATA: mo_database_access TYPE REF TO lif_database_access,
          mo_missile_control TYPE REF TO lif_missile_control,
          mo_user_interface  TYPE REF TO lif_user_interface,
          mo_printer         TYPE REF TO lif_printer.
ENDCLASS.

CLASS lcl_missile_firer IMPLEMENTATION.
*--------------------------------------------------------------------*
* Listing 4.13: Revised Constructor Implementation
*--------------------------------------------------------------------*
  METHOD constructor.

    DATA(lo_factory) = lcl_monster_factory=>get_instance( ).

    mo_database_access = lo_factory->get_database_access( ).
    mo_missile_control = lo_factory->get_missile_control( ).
    mo_user_interface  = lo_factory->get_user_interface( ).
    mo_printer         = lo_factory->get_printer( ).

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
* Automated Unit Tests
*--------------------------------------------------------------------*
* Listing 04.16 : Test Class Defintion - General Settings
*--------------------------------------------------------------------*
CLASS ltc_missile_firer DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.
*--------------------------------------------------------------------*
* Listing 04.17 : Defining Test Doubles for Injection into CUT
*--------------------------------------------------------------------*
  PRIVATE SECTION.
    DATA: mo_cut                TYPE REF TO lcl_missile_firer,
          mo_td_database_access TYPE REF TO ltd_database_access,
          mo_td_missile_control TYPE REF TO ltd_missile_control,
          mo_td_user_interface  TYPE REF TO ltd_user_interface,
          mo_td_printer         TYPE REF TO ltd_printer.

    METHODS:
      setup,
      test_something FOR TESTING.
ENDCLASS.

CLASS ltc_missile_firer IMPLEMENTATION.
  METHOD setup.
    "Create Test Doubles

    "Creat Instance of Class Under Test
    mo_cut = NEW #( ).

  ENDMETHOD.

  METHOD test_something.
* Given
* Set Up Input Data

* When
* Call Production Code to be Tested
    mo_cut->fire_nuclear_missile( ).

* Then
* Do some evaluations (assertions) to see if the code worked
  ENDMETHOD.
ENDCLASS."Test Class
*--------------------------------------------------------------------*
* Listing 04.06 : Calling a Method instead of a FORM Routine
*--------------------------------------------------------------------*
START-OF-SELECTION.
  NEW lcl_missile_firer( )->fire_nuclear_missile( ).
