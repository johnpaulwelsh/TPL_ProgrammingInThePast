object Cipher {
    def encrypt(inputStr: String, shiftAmt: Int):String = {
        var outputStr:String = ""
        var newCh = ' '
        var newCode = 0

        for(chCount <- 0 until 15) {
            newCh = inputStr.charAt(chCount)
            newCode = newCh.asInstanceOf[Int]
            newCode = newCode + shiftAmt
            newCh = newCode.asInstanceOf[Char]
            outputStr = outputStr + newCh
        }

        return outputStr
    }

    def decrypt(inputStr: String, shiftAmt: Int):String = {
        var outputStr:String = ""
        var newCh = ' '
        var newCode = 0

        for(chCount <- 0 until 15) {
            newCh = inputStr.charAt(chCount)
            newCode = newCh.asInstanceOf[Int]
            newCode = newCode - shiftAmt
            newCh = newCode.asInstanceOf[Char]
            outputStr = outputStr + newCh
        }

        return outputStr
    }

    def main(args: Array[String]) {
        var inputStr = "John Paul Welsh"
        var shiftAmt = 1
        var maxShiftVal = 26
        
        inputStr = inputStr.toUpperCase()

        println("The input string is " + inputStr)
        println("The shift amount for encrypt and decrypt is " + shiftAmt)
        println("The maximum shift value for solve is " + maxShiftVal)
        println("Encrypted: " + encrypt(inputStr, shiftAmt))
        println("Decrypted: " + decrypt(inputStr, shiftAmt))
        //solve(inputStr, maxShiftVal))
    }
}