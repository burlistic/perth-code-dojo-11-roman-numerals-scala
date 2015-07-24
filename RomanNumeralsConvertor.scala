class RomanNumeralsConvertor {

	def BuildConverter(base: Int, small: String, medium: String, large: String): (Int => String) =
	{
		(arabicNumber: Int) =>
		if (arabicNumber % (base * 10) >= base){
    	 	Digits(arabicNumber % (base * 10) / base, small, medium, large)
    	}else{""}
	}

	val converters = List(
		BuildConverter(1000, "M", "", ""),
		BuildConverter(100, "C", "D", "M"),
		BuildConverter(10, "X", "L", "C"),
		BuildConverter(1, "I", "V", "X")
		)

    def Convert(arabicNumber: Int): String = {

		converters.foldLeft("")(
			(acc, 
		  converter: (Int => String)) => acc + 
		  converter(arabicNumber))
    }

    def Digits(digit: Int, small: String, medium: String, large: String): String = {
    	// assume(digit < 10, "Only single digits allowed")

    	digit match {
    		case d if d < 4 => small * d// e.g. "X" * 2 == "XX"
    		case d if d == 4 => small + medium
    		case d if d < 9 => medium + (small * (d - 5))
    		case d if d == 9 => small + large
    		case _ => ""
    	}
    } 

    def Test(input:Int, expected:String) = {

    	val output = Convert(input)

		if(output != expected)
		{
			println("Test failed. Input " + input + 
				" Output: " + output + 
				" Expected: " + expected)


		}

		println("Test passed. Input " + input + " Output: " + output)
    }
}

val romanNumeralsConvertor = new RomanNumeralsConvertor()

romanNumeralsConvertor.Test(1,"xxx");
romanNumeralsConvertor.Test(2,"II");
romanNumeralsConvertor.Test(3,"III");
romanNumeralsConvertor.Test(4,"IV");
romanNumeralsConvertor.Test(5,"V");
romanNumeralsConvertor.Test(6,"VI");
romanNumeralsConvertor.Test(7,"VII");
romanNumeralsConvertor.Test(8,"VIII");
romanNumeralsConvertor.Test(9,"IX");
romanNumeralsConvertor.Test(10,"X");
romanNumeralsConvertor.Test(11,"XI");
romanNumeralsConvertor.Test(20,"XX");
romanNumeralsConvertor.Test(30,"XXX");
romanNumeralsConvertor.Test(40,"XL");
romanNumeralsConvertor.Test(50,"L");
romanNumeralsConvertor.Test(60,"LX");
romanNumeralsConvertor.Test(70,"LXX");
romanNumeralsConvertor.Test(80,"LXXX");
romanNumeralsConvertor.Test(90,"XC");

romanNumeralsConvertor.Test(99,"XCIX");
romanNumeralsConvertor.Test(100,"C");

romanNumeralsConvertor.Test(124,"CXXIV");
romanNumeralsConvertor.Test(500,"D");
romanNumeralsConvertor.Test(600,"DC");
romanNumeralsConvertor.Test(666,"DCLXVI");
romanNumeralsConvertor.Test(1000,"M");
romanNumeralsConvertor.Test(3000,"MMM");
romanNumeralsConvertor.Test(3100,"MMMC");