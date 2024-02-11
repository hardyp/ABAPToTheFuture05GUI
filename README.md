# ABAPToTheFuture05GUI

ABAP to The Future
Episode Five: Merrily we ROLLBACK WORK
SAP GUI Edition
Baron Frankenstein has returned. The fifth edition of “ABAP to the Future” will be released early in 2027.
At time of writing (Feb 2024) that seems quite a long way off. The idea (from SAP Press) is to give SAP a couple of years to come up with new and exciting things, because at the moment all the new and exciting things in ABAP world are coming from the open-source community.
However, I see no logical reason not to start working on the book and the code right away. If anything it is better because I do not have the tight deadlines breathing over my shoulder.
This time there will be two GitHub repositories.
One (this one) will contain code from the latest ABAP trial system (7.54 at time of writing) and the other will contain code from an ABAP in the Cloud system a.k.a. SAP BTP ABAP Environment a.k.a. Steampunk.
I will try and maintain functional parity between the two, but the obvious difference is the former can run in the SAP GUI and the latter must be all web-based applications.
There will be three sorts of programs.
1.	Utility programs – to generate test data and to drill into the code listings.
2.	Example programs which do not do anything except provide a home for various code listings.
3.	The business applications used by Baron Frankenstein and his Hunchbacks. The idea is that these contain the exact same code samples used in the example programs. Some of the examples are “do not do this” examples so they won’t be in the applications.
This time we are going to have an end-to-end test script to demonstrate how to use the various applications to build a monster and then use it to terrorize the peasants. I never did that in the previous books, you just had to sort of imagine how the business process worked.
I will upload a spreadsheet to both repositories showing the test-driven development process of building the applications for the highly realistic business model used by Baron Frankenstein, far more realistic than SFLIGHT. That is, you start with user stories to define the business requirements, design SAP applications based on those user stories, and then define a test script for the end-to-end business process using those applications. Only then do you build the applications, in a TDD manner.
I will be uploading the code to GitHub as I am writing the book, so feel free to tell me anything I have got wrong or am doing in a stupid way.
Cheersy Cheers
Paul
