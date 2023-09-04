# see: https://www.imsglobal.org/question/qtiv1p2/imsqti_asi_bindv1p2.html

library(openxlsx)
library(pracma)

source('qti_build.R')

filesep<-"/"

# % define the quiz
quiz.title='ExQuizR'
quiz.noQuestions=5
quiz.questions2Answer=2

question.noChoicesPerAnswer=4
question.answerMargin=0.05 # for numerical answers

# choose one of these:
# 'multiple_dropdowns_question'
# 'multiple_choice_question'
# 'fill_in_multiple_blanks_question'
#'single_numerical_question'
question.type='multiple_dropdowns_question'

# numerical is not working for more than 1 value per question (Canvas have not implemented this):
# question.type='multiple_numerical_question'

##############################
# now we make the quiz as specified

# % make a folder for the quiz materials and the data files
folder<-quiz.title
# remove the folder if it already exists to ensure clean result
if (dir.exists(folder)) {
  system(paste0("rm -r ", folder))
}
# top level folder
dir.create(folder)
# inside folder
dir.create(paste0(folder, filesep, folder))


# % make the specific questions
for (qi in 1:quiz.noQuestions) {
  # % make some data for the quiz
  data<-data.frame(x=rnorm(20))
  mnData=mean(data$x) 
  sdData=sd(data$x)
  # save the data in an excel file
  datafile=paste0(quiz.title, '_', 'data', qi, '.xlsx')
  write.xlsx(data,file=paste0(folder,filesep, datafile))
  
  # text for question 
  # for checking purposes, I have included the correct answer
  question.text<-c(
    paste0('Download the data file from: <a href="', datafile, '">here</a>'),
    paste0('Analyse it to calculate the mean (', format(mnData,digits=3), ') and standard deviation (', format(sdData,digits=3), ').'),
    'Then choose which of these is correct:'
  )
  nqt<-length(question.text)
  
  # for each question, we make
  # the text for the question
  # the keywords (that identify the answers)
  # the values for the 1st answer
  #  the first answer is the correct one
  #  subsequent answers are the other choices available (where appropriate)
  # then the values for the second answer (where appropriate)
  switch(question.type,
         'multiple_dropdowns_question'={
           question.text[nqt+1]=' mean=[meanValue]  and sd=[sdValue]'
           question.keywords<-c('meanValue',  'sdValue')
           
           #% Answer 1: mean answer
           #% correct answer goes first
           question.answers=format(mnData,digits=3)
           #% then alternative choices (random numbers)
           for (i in 2:question.noChoicesPerAnswer){
             question.answers=c(question.answers,format(mnData+rnorm(1)*sdData,digits=3))
           }
           
           #% Answer 2: sd answer
           question.answers2=format(sdData,digits=3)
           #% then alternative choices (random numbers)
           for (i in 2:question.noChoicesPerAnswer) {
             question.answers2=c(question.answers2,format(sdData+rnorm(1)*sdData,digits=3))
           }
         },
         
         'fill_in_multiple_blanks_question'={
           question.text[nqt+1]=' mean=[meanValue]  and sd=[sdValue]'
           question.keywords<-c('meanValue',  'sdValue')
           
           #% Answer 1: mean answer
           #% correct answer goes first
           question.answers=format(mnData,digits=3)

           #% Answer 2: sd answer
           question.answers2=format(sdData,digits=3)
           
         },
         
         'multiple_choice_question'={
           question.keywords<-c()
           
           question.answers=paste0('MN(', format(mnData,digits=3), ') SD(', format(sdData,digits=3), ')')
           for (i in 2:question.noChoicesPerAnswer) {
             question.answers= cbind(question.answers,paste0('MN(', format(mnData+randn(1)*sdData,digits=3), ') SD(', format(sdData+randn(1)*sdData,digits=3), ')'))
           }
           
           question.answers2<-c()
         },
         
         
         'single_numerical_question'={
           question.text<-question.text[1:(nqt-1)]
           question.text[3]='Write the value for the mean in this space:'
           question.keywords<-c('meanValue')
           
           #% Answer 1: mean answer
           question.answers=mnData
           
           question.answers2=c()
           
         }
  )
  # now we bind it all together into a variable called questions
  if (!is.null(question.answers2)) {
    answers=rbind(question.answers,question.answers2)
    errorMargin=rbind(question.answerMargin,question.answerMargin)
  } else {
    answers=rbind(question.answers)
    errorMargin=rbind(question.answerMargin)
  }
  q<-list(
    type=question.type,
    text=question.text,
    keywords=question.keywords,
    answers=answers,
    errorMargin=errorMargin,
    linkedFiles=datafile
  )
  q1<-array(q,dim=c(1,length(q)))
  colnames(q1)<-names(q)
  if (qi==1) {
    questions<-q1
  }  else {
    questions<-rbind(questions,q1)
  }
}
# questions all made

# make a variable quiz that holds everything needed
quiz<-list(title=quiz.title,
           questions=questions,
           questions2Answer=quiz.questions2Answer
)

# call the function that builds the quiz
qti_build(quiz,folder)

# the result is a .zip file that can be imported into Canvas
# it sets the following general quiz properties:
#    the quiz description states the need for 3 sig fig
#    the quiz is set up to randomise the order of answer choices (this is necessary)
#    correct answers are shown at the end of the quiz
#    one question is shown at a time when running the quiz
# all of these can be changed if desired


