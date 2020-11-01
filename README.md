# WormSorter
Very cool. 

## 21.10.20
Hey Khlifa! I hope you are doing fine and that you dont mind I modify slightly your first repo; I truly think small changes will make your life easier in the future ^^. 
Also, I think we can use the readme file as alternative way of communication, it should help us both to get used to the markdown language. Feel free to modify it to make clear your updates.

**Structure of a repos**
There is not a single gold standart on how to mantain organization on a project. Each project is different and will not be similar to each other, nonetheless, you can follow simple guidelines so your future self will thank you for doing things somewhat right since the beginning. 
I recommend you to start by reading [this](https://www.r-bloggers.com/2018/08/structuring-r-projects/).

I made some initial directories and placed files on it for you, but please feel free to arrange them according to your ideas/needs:
- data `directory containing profiles`
- Rsrc `directory containing your functions and source files`
- analysis `directory with R scripts used in your analysis`
- original_files `extra archive for the files you had on your first commit`

Finally, I created a file named 1_updated_functions.R which contains my comments regarding the functions you created. Overall they had an easy logic to understand, but I think they can be slightly improved.

More to come!! (I should contact you latter today regarding the SVM; the S did not stand for Supervized nor Standarized, but for support ^^').

## 23.10.20

Hello Amhed! I'm still trying to figure out how to work with git hub but it's very exciting. 
For some reason the suggestions you had for the main function Rsrc/ReadDataFunc  did not work, which.min always gave off the index 1 instead of the actual value but which.max worked fine. 
I also changed the script analysis/TestCh.R slightly and added a plot function that plots the mean vs the Id of the worm with ggplot (it's very weird compared to normal plot functions on matlab lol)
I wish I had more time to work on in this weekend but I have a couple of important deadlines due. I'll check out the SVM as soon as I have time. 


## 29.10.20

Hey Khlifa! HOw are you doing and how did you find Adamala's presentation? It's really cool the techniques/methods they are developing isn't?. I bet we can convice Christian to try some of them :).

So, I review your plotting code and fix part of my mistakes of my R functions (sorry for that). You can find my review in: 
`analysis/Test_read-profile_TASKs.R`
Also, I added notes and wrote the kind of expected function/pipe required to do a plotting GUI interface as Sonia suggested. In order to test the plotting of multiple profiles, I added to the data folder the N2 profiles that Sonia gave us previously.

Please dont be afraid to ask any doubt, inquiry or even spot and blame my mistakes hahaha :). Similarly, remember that there is no rush and we will do according to your schedule.

Cheers!

Amhed

## 01.11.20

Hey Amhed! 
The presentation was very interesting! 
I created a simple function called 'ReadProfile' that takes in the filename and required ID 
So far it returns a dataframe that follows the same format you had in your review in 'Test_read-profile_TASKs.R' so the result will be easily plotted using ggplot 

Best regards,
Khlifa 

## 0.1.11.20
Hello again! 
I set up the notification so that we recieve an email after each push. 

Best, 
Khlifa