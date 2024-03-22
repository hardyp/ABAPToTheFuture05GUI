*&---------------------------------------------------------------------*
*& Report Z5_TDD_DEPENDENCIES_V02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
* Listing 05.02: - Common ABAP Application
*--------------------------------------------------------------------*
* In this example we are not allowed to redesign the program, so we
* surround each dependency with a TEST SEAM to (a) identify the
* dependency for what it is, and (b) allow the production code to
* be used in automated unit tests
* In real life, only the maddest of lunatics would do this, and then
* only reluctantly
*--------------------------------------------------------------------*
REPORT z5_tdd_dependencies_v02.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  "Test Seams only work in Function Groups or Class Pools
  CALL FUNCTION 'Z5_TEST_SEAMS'.
