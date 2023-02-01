# mainfraime
Programa feito para Sprint do projeto Capflix

Este programa tem como objetivo registrar um usuário. Ele deve receber e validar os dados do usuário, e registrar em arquivo indexado. Este é um programa online, que recebe as seguintes informações do usuário:
•	Email                        PIC  X(100)
•	name                        PIC  X(100)
•	password :               PIC  X(8)
•	phone:             	      PIC  9(12)
Fluxo: 
O usuário acessa a interface, preenche os dados citados.
O programa realiza as seguintes validações:
1.	Email é válido (ter pelo menos 10 caracteres, ter um caractere “@”, ter pelo menos um caracter antes do @ e pertencer aos domínios “capgemini.com” ou “bradesco.com”
2.	Nome com pelo menos duas palavras 
3.	Senha com pelo menos 8 caractesres, 1 número, 1 letra maiúscula, 1 letra minúscula e um caracter especial
4.	Telefone com no mínimo 11 caracteres e maximo 12 caracteres. Ex: (999999999999). 
Caso algum dos campos esteja inválido, o programa deve apresentar um erro e uma sugestão de correção ao usuário. Em caso de sucesso, o programa chama o módulo de geração de ID e registra o usuário, indexado pelo id gerado. Ex:


ID               nome            email                                            phone
0001          Carlos           carlos@capgemini.com           756666666666
0002          Carlos           carlos@capgemini.com           756666666666
0003          Carlos           carlos@capgemini.com           756666666666
0004          Carlos           carlos@capgemini.com           756666666666
