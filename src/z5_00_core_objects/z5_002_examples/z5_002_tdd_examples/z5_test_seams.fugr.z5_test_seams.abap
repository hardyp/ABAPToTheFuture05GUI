FUNCTION z5_test_seams.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  PERFORM fire_nuclear_missile.

ENDFUNCTION.
*--------------------------------------------------------------------*
* Sub-Routines
*--------------------------------------------------------------------*
* Listing 04.02 : Surrounding Dependencies with Test Seams
*--------------------------------------------------------------------*
FORM fire_nuclear_missile.

  TEST-SEAM read_database.
* Here the dependency is needing an actual database with real data
    CALL FUNCTION 'Z5READ_MONSTER_CUSTOMIZING'.
  END-TEST-SEAM.

  TEST-SEAM missile_sensor.
* Here the dependency is needing contact with an actual missile system
    CALL FUNCTION 'Z5GET_NUCLEAR_MISSILE_SSENSOR'.
  END-TEST-SEAM.

* Actual Business Logic (that you want to test)
* You would want to test that the missile gets sent to the right place
* i.e. gets dropped on your enemy, not on you
* IF something.
*   "We aim the missile here
* ELSEIF something_else.
*   "We aim the missile here
* ENDIF.

* Ask the user if they want to fire the missile
  TEST-SEAM user_input.
    DATA: user_answer TYPE char01.
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

  END-TEST-SEAM.

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

  TEST-SEAM missile_launch.
* Here the dependency is needing an actual missile to fire
    CALL FUNCTION 'Z5TELL_PROXY_2_FIRE_MISSILE'.
  END-TEST-SEAM.

  TEST-SEAM printer.
* Here the dependency is needing an actual printer
    CALL FUNCTION 'Z5PRINT_NUCLEAR_SMARTFORM'.
  END-TEST-SEAM.

ENDFORM.                    "fire_nuclear_missile
