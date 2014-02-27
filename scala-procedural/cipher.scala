object Cypher {

    def encrypt(inputStr: String, shiftAmt: Integer): String = {
        var outputStr = inputStr
        var len = inputStr.length()
        var chCount = 0
        var newCh, newCode
        
        for (chCount <- 0 until len) {
            newCh = inputStr.charAt(chCount)
            newCode = newCh
            newCode = newCode + shiftAmt
            newCh = newCode
            outPutStr.charAt(chCount) = newCh
        }
        
        return outputStr
    }

    def main(args: Array[String]) {
        var inputStr = "John Paul Welsh"
        var shiftAmt = 15
        var maxShiftVal = 26
        
        inputStr = inputStr.toUpperCase()

        println(encrypt(inputStr, shiftAmt))
    }
}
