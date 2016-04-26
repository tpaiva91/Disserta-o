%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                            %
%                                           Advice Messages                                                  %
%                                                                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%In this file Messages are inserted or defined
% messages must have the format msg(ID, Message with extra options separated by "-")

msg('hasHighGlucose', 'O seu valor Glicemico encontra-se alto. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-Glycemia-600-s/').
msg('hasLowGlucose', 'O seu valor Glicemico encontra-se baixo. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-Glycemia-600-s/').

msg('hasHighHbA1c', 'O seu valor de HbA1c encontra-se alto. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-HbA1c-600-s/').
msg('hasLowHbA1c', 'O seu valor de HbA1c encontra-se baixo. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-HbA1c-600-s/').

msg('hasHighCholesterol', 'O seu valor de Colesterol encontra-se alto. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-Cholesterol-600-s/').
msg('hasLowCholesterol', 'O seu valor de Colesterol encontra-se baixo. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-Cholesterol-600-s/').

msg('hasHighInsulin', 'O Valor de insulina inserido � muito alto. Um teste ir� ser agendado para assegurar que o valor inserido n�o criou uma crise de hipoglicemia./adviceTimer-Glycemia-600-s/').

msg('hasHighWeight', 'O seu peso encontra-se muito alto. Por favor tenha cuidado com o seu peso./simpleAdvice/').
msg('hasLowWeight', 'O seu peso encontra-se muito baixo. Por favor tenha cuidado com o seu peso./simpleAdvice/').

msg('hasHighArterialPressure', 'O seu valor de press�o arterial encontra-se alto. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-BloodPressure-600-s/').
msg('hasLowArterialPressure', 'O seu valor de press�o arterial encontra-se baixo. Um novo teste ir� ser agendado para verificar se este valor estabiliza./adviceTimer-BloodPressure-600-s/').

msg('mealExercisedRecently', 'Recentemente fez exerc�cio f�sico. Tenha em aten��o que o valor de insulina sugerido pode n�o corresponder � dose real, pois o seu c�lculo � feito sem ter em conta o exerc�cio f�sico./simpleAdvice/').
msg('mealUserWithHighWeight', 'Modere as suas refei��es e tenha cuidado com o seu peso./simpleAdvice/').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                            %
%                                             Task Division                                                  %
%                                                                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------------------------------------
%% Essencial Task messages
%%------------------------------------------------------------------------------------------------------------

msg('Tsk_glycemiaReg', 'J� n�o testa a sua Glicemia � demasiado tempo. O �ltimo valor registado tem o valor expirado. Por favor fa�a um novo teste./10').
msg('Tsk_hbA1cReg', 'J� n�o testa o seu valor de HbA1c � demasiado tempo. O �ltimo valor registado tem o valor expirado. Por favor fa�a um novo teste./10').
msg('Tsk_arterialPReg', 'J� n�o testa o seu valor de Press�o Arterial � demasiado tempo. O �ltimo valor registado tem o valor expirado. Por favor fa�a um novo teste./10').
msg('Tsk_weightReg', 'J� n�o mede o seu Peso � demasiado tempo. O �ltimo valor registado tem o valor expirado. Por favor fa�a uma nova pesagem./10').
msg('Tsk_cholesterolReg', 'J� n�o testa o seu Colesterol � demasiado tempo. O �ltimo valor registado tem o valor expirado. Por favor fa�a um novo teste./10').

%%------------------------------------------------------------------------------------------------------------
%% Other Task messages
%%------------------------------------------------------------------------------------------------------------

msg('Tsk_doExerciseTwoTimesADay', 'Deve fazer Exerc�cio F�sico e regista-lo, pelo menos duas vezes ao dia.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                            %
%                                            Personalized Tasks                                              %
%                                                                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------------------------------------
%% Defines the text of the possible tasks and their parameter
%%------------------------------------------------------------------------------------------------------------

msg(tskPers1, glicemia, 'Testar Glicemia').
msg(tskPers2, exercise, 'Registar novo Exercicio').
msg(tskPers3, insulin, 'Registar nova dose de Insulina').
msg(tskPers4, meal, 'Registar nova Refei��o').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                            %
%                                             Null Message                                                   %
%                                                                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Default Message in case of no advice triggered
msg('NULL', '').