       *> Tyler Zysberg
       *> A program to report the number of errors from an infile
       
       identification division.
       program-id.     Lab5.
       
       Environment division.
       configuration section.
       special-names.
         class genderclass is "F", "f", "M", "m", "U", "u", "R", "r"
         class MaritalClass is "D", "d", "M", "m", "P", "p", "S",
            "s", "W", "w"
         class PayCodeClass is "C", "c", "H", "h", "S", "s".
       
        input-output section.
        file-control.
            select infile assign to "lab5-in.dat"
                organization is line sequential.
                
            select ot-rpt assign to "lab5-out.dat"
                organization is line sequential.
        data division.
        file section.
        
         fd  infile.
         01  in-record.
           05 RegionNum              PIC X(2).
           05 RegionName              PIC X(15).
           05 DepartmentNum          PIC X(5).
           05 DepartmentName         Pic X(30).
           05 Employee-Number        Pic X(5).
           05 LastName               PIC X(20).
           05 FirstName               PIC X(15).
           05 Gender                 Pic X(1).
           05 Adress                  PIC X(20).
           05 CityState              PIC X(20).
           05 JobTitle               PIC X(20).
           05 DOB                    Pic 9(8).
           05 DOH                    PIC 9(8).
           05 Marital                pic X(1).
           05 Dependents             PIC 9(2).
           05 SchoolDistrict.
             10 SchoolDistrict1      pic 9.
             10 SchoolDistrict2      pic 9.
             10 SchoolDistrict3      pic 9.
           05 medIns                  PIC X.
           05 denIns                  PIC X.
           05 visIns                  PIC X.
           05 401k                    PIC 9(3).
           05 PayCode                pic X(1).
           05 PayRate                pic S9(9).
           05 Hours-Per-week         Pic S9(2)v99.
           05 commis                 PIC S9(3).
           05 ActualSales             PIC S9(9).
           
           
           fd  ot-rpt.
           01  ot-record              pic x(207).
        
        working-storage section.
        01  eof                    pic x value "N".
        
        01  counter                Pic 9(3) value 0.
        01  total-counter          Pic 9(5) value 0.
        01  total-record-errors    Pic 9(5) value 0.
        01  crt-date.
            05 crt-year            pic 9(4).
            05 crt-month           pic 9(2).
            05 crt-day             pic 9(2).
        78  newline           value x"0a".
        01  yeardiff               pic S9(8).
        
        procedure division.
        000-main.

           perform 100-initialize
           
           perform until eof = "y"
              read infile
                 at end
                    move "y" to eof
                 not at end
                    perform 300-process
                    if counter > 0
      * Display the line
                       write ot-record from in-record
                       write ot-record from newline
                       add 1 to total-record-errors
                    end-if
                    add counter to total-counter
                    move 0 to counter
              end-read
           end-perform
           
          write ot-record from "Number of records with errors: "
          write ot-record from total-record-errors
          write ot-record from "Total number of errors: "
          write ot-record from total-counter

                     
           perform 900-finalize
                     
           stop run.
           
        100-initialize.
           open input infile
              output ot-rpt.
              
        300-process.
        
        Accept crt-date from date YYYYMMDD

      * Non-numeric Employee Number
      * Class Test
          If Employee-Number is Not Numeric 
          write ot-record from "Non-numeric Employee Number found:"
           add 1 to counter
          Else 
            Continue
          End-If
          
      * Non-alphabetic Department Name
      
          If DepartmentName is not Alphabetic
          write ot-record from "Non-alphabetic Department Name found:"
           add 1 to counter
          Else 
            Continue
          End-If
          
      * Invalid Gender Code
      
          If Gender Is Not genderclass
          write ot-record from "Invalid Gender found:"
           add 1 to counter
          Else 
           Continue
          End-If
      
      *  Invalid Marital Status
      
          If Marital Is Not MaritalClass
          write ot-record from "Invalid Marital found:"
           add 1 to counter
          Else 
           Continue
          End-If
          
      *  Invalid Paycode
      
          If PayCode Is Not PayCodeClass
          write ot-record from "Invalid Paycode found:"
           add 1 to counter
          Else 
           Continue
          End-If
      
      * Non-numeric Hours-Per-week
      
          If Hours-Per-week Is Not Numeric 
          write ot-record from "Non-numeric Hours-Per-week found:"
           add 1 to counter
          Else
            Continue
          End-If
      
      * Negative Hours-Per-week  
      
          If Hours-Per-week Is Negative and Hours-Per-week is 
         numeric
          write ot-record from "Negative Hours-Per-week found:"
           add 1 to counter
          Else
            Continue
          End-If
          
      * Excessive Hours-Per-week  
      
          If Hours-Per-week Is >60 and Hours-Per-week is 
         numeric
          write ot-record from "Excessive Hours-Per-week found:"
           add 1 to counter
          Else
            Continue
          End-If
          
      * Non-numeric PayRate        
        
        If PayRate Is Not Numeric 
          write ot-record from "Non-numeric PayRate found:"
           add 1 to counter
         Else if
          PayRate Is Negative
          write ot-record from "Negative PayRate found:"
           add 1 to counter
         else continue
          End-If
          
      * Non-Numeric DOH
       
         If DOH is not numeric
          write ot-record from "Hire Date is not a valid date found:"
           add 1 to counter
         Else
           If Function Test-Date-YYYYMMDD(DOH) not equal 0 
            and DOH is numeric
            write ot-record from "Hire Date is not a valid date found:"
             add 1 to counter
           Else
              If DOH is greater than crt-date
              write ot-record from "Hire Date is in the future found:"
                  add 1 to counter
              Else
		 
         
      *> Hire date is less than 18 years
         compute yeardiff = DOB + 180000
         if DOH < yeardiff
         write ot-record from 
		    "Hire Date is less than 18 years found:"
             add 1 to counter
         else 
         End-If.
        
      *> School district errors
	  
        
         
       900-finalize.   
           close infile ot-rpt.
         
         
         
         
          
          
          
          
      
        