// Caesar Cipher - Scala (procedural)
// Author: John Paul Welsh
object Cipher {
    def encrypt(inputStr: String, shiftAmt: Int):String = {
        var outputStr = ""
        var newCh = ' '
        var newCode = 0

        for(chCount <- 0 until 15) {
            newCh = inputStr.charAt(chCount)
            newCode = newCh.asInstanceOf[Int]
            if (newCode != 32) {
                newCode = newCode + shiftAmt
                if (newCode > 90) newCode = 64 + (newCode % 90)
                if (newCode < 65) newCode = 91 - (65 % newCode)
                newCh = newCode.asInstanceOf[Char]
            }
            outputStr = outputStr + newCh
        }

        return outputStr
    }

    def decrypt(inputStr: String, shiftAmt: Int):String = {
        var outputStr = ""
        var newCh = ' '
        var newCode = 0

        for(chCount <- 0 until 15) {
            newCh = inputStr.charAt(chCount)
            newCode = newCh.asInstanceOf[Int]
            if (newCode != 32) {
                newCode = newCode - shiftAmt
                if (newCode > 90) newCode = 64 + (newCode % 90)
                if (newCode < 65) newCode = 91 - (65 % newCode)
                newCh = newCode.asInstanceOf[Char]
            }
            outputStr = outputStr + newCh
        }

        return outputStr
    }

    def solve(inputStr: String, maxShiftVal: Int) {
        var shift = 0
        var newCh = ' '
        var newCode = 0

        for(iteration <- maxShiftVal to 0 by -1) {
            var outputStr = ""
            // there were issues with maxShiftVal being > 26, so I just
            // simplified the procedure be modding the current iteration
            shift = iteration % 26

            for(chCount <- 0 until 15) {
                newCh = inputStr.charAt(chCount)
                newCode = newCh.asInstanceOf[Int]
                if (newCode != 32) {
                    newCode = newCode - shift
                    if (newCode > 90) newCode = 64 + (newCode % 90)
                    if (newCode < 65) newCode = 91 - (65 % newCode)
                    newCh = newCode.asInstanceOf[Char]
                }
                outputStr = outputStr + newCh
            }
            println("Caesar " + iteration + ": " + outputStr)
        }
    }

    def main(args: Array[String]) {
        var inputStr = "John Paul Welsh"
        var shiftAmt = -6
        var maxShiftVal = 28
        
        inputStr = inputStr.toUpperCase()

        println("The input string is " + inputStr)
        println("The shift amount for encrypt and decrypt is " + shiftAmt)
        println("The maximum shift value for solve is " + maxShiftVal)
        println("Encrypted: " + encrypt(inputStr, shiftAmt))
        println("Decrypted: " + decrypt(inputStr, shiftAmt))
        solve(inputStr, maxShiftVal)
    }
}
