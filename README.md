# PRICE_OPTIMIZATION

	
 This project consist in predict the 'account value' of	a	given	account	with the data that the users provide during the online application and the initial quotes given. This prediction is one of the core parts of the lead scoring model. 
  
	
## **Data**
  
Two data sets:	accounts and	quotes.	
  
  
* *Accounts:* contains a sample of our customers with the information they provided in the initial application form.	
  
* *account_uuid:* uuid of the account	
  
* *state:* state of the business	
  
* *industry:* industry of the business. When blank, not indicated.	
  
* *subindustry:* subindustry of the business. When blank, not indicated	
  
* *year_established:* year the business was created	
  
*	*annual_revenue:*	annual	revenue	of	the	business	
  
*	*total_payroll:*	total	payroll	to	the	workers	of	the	business	
  
*	*business_structure:*	type	of	business	
  
*	*num_employees:*	total	number	of	employees	of	the	business	
  
* *Quotes:*	contains	the	quotes	that	were	given	to	the	user	after	submitting	the	on-line	application	form.	
  Some	of	the	quotes	is	what	the	user	decided	to	finally	buy.	
  
*	*account_uuid:*	uuid	of	the	account	
  
*	*product:*	product	type	the	user	has	requested	
  
*	*premium:*	price	given	to	the	product	
  
*	*carrier_id:*	insurance	carrier	that	provides	the	product	
  
*	*convert:*	1	if	the	user	has	bought	the	product,		otherwise.	This	variable	is	not	provided	in	the	test	data.	
  
The	account	value	of	a	given	account	is	defined	as	the	sum	of	the	premium	of	those	products	that	the	user	has	bought	(convert==1).	See	example_account_value.csv	
  

