/* The query asked in question 14.3.1 */
answer(G) :- csg('PH100', S, G), snap(S, 'L. Van Pelt', A, P).

/* Call as follows: answer(G). */

/* Proving the facts in 14.9.2 */
before(X, Y) :- cp(X, Y).
before(X, Y) :- cp(X, Z), before(Z, Y).

/* Call as follows: before('CS120', 'CS100'). */
/* Call as follows: before('CS206', 'CS100'). */

/* Figure 8.1 from FCS */
csg('CS101', 12345, 'A').
csg('CS101', 67890, 'B').
csg('EE200', 12345, 'C').
csg('EE200', 22222, 'B+').
csg('CS101', 33333, 'A-').
csg('PH100', 67890, 'C+').

/* Figure 8.2a from FCS */
snap(12345, 'C. Brown', '12 Apple St.', '555-1234').
snap(67890, 'L. Van Pelt', '34 Pear Ave.', '555-5678').
snap(22222, 'P. Patty', '56 Grape Blvd.', '555-9999').

/* Figure 8.2b from FCS */
cp('CS101', 'CS100').
cp('EE200', 'EE005').
cp('EE200', 'CS100').
cp('CS120', 'CS101').
cp('CS121', 'CS120').
cp('CS205', 'CS101').
cp('CS206', 'CS121').
cp('CS206', 'CS205').

/* Figure 8.2c from FCS */
cdh('CS101', 'M', '9AM').
cdh('CS101', 'W', '9AM').
cdh('CS101', 'F', '9AM').
cdh('EE200', 'Tu', '10AM').
cdh('EE200', 'W', '1PM').
cdh('EE200', 'Th', '10AM').

/* Figure 8.2d from FCS */
cr('CS101', 'Turing Aud.').
cr('EE200', '25 Ohm Hall').
cr('PH100', 'Newton Lab.').
