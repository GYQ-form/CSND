#simulation of student choosing their courses
StudentChooseClass <- function(){
  studentID <- as.character(sample(100000:999999,100))
  Chinese <- sample(0:1,100,replace = T)
  Mathematics <- sample(0:1,100,replace = T)
  English <- sample(0:1,100,replace = T)
  Physical <- sample(0:1,100,replace = T)
  Chemistry <- sample(0:1,100,replace = T)
  Biology <- sample(0:1,100,replace = T)
  History <- sample(0:1,100,replace = T)
  Geography <- sample(0:1,100,replace = T)
  StdClsChos <- data.frame(studentID,Chinese,Mathematics,English,Physical,Chemistry,History,Geography,Biology)
  rownames(StdClsChos) <- paste("student",1:100,sep = "")
  write.csv(StdClsChos,"StudentClassChoose.csv")
}

#simulation of grades of students' course
RandomCourseGrades <- function(){
  StdClsChos <- read.csv("StudentClassChoose.csv",header = T,row.names = 1)
  CourseGrades <- StdClsChos
  for(i in 1:nrow(CourseGrades)){
    for(j in 2:ncol(CourseGrades)){
      if(StdClsChos[i,j]==1)CourseGrades[i,j] <- sample(0:100,1)
      else if(StdClsChos[i,j]==0)CourseGrades[i,j] <- NA
    }
  }
  write.csv(CourseGrades,"CourseGrades.csv")
}

#simulation of course information
RandCourseInfo <- function(){
  AllTeacher <- paste("teacher",1:9,sep = "")
  course <- c("Chinese","Mathematics","English","Physical","Chemistry","History","Geography","Biology")
  teacher <- sample(AllTeacher,8,replace = T)
  credit <- sample(5:8,8,replace = T)
  CourseInfo <- data.frame(credit,teacher)
  row.names(CourseInfo) <- course
  write.csv(CourseInfo,"CourseInfo.csv")
}

#simulation of all teacher information
RandTeacherInfo <- function(){
  CourseInfo <- read.csv("CourseInfo.csv",header = T,row.names = 1)
  ID <- as.character(sample(1000:9999,9))
  AllTitle <- c("pro","resercher","assistant","lecturer")
  Title <- sample(AllTitle,9,replace = T)
  AllTeacher <- paste("teacher",1:9,sep = "")
  course <- rep("",9)
  TeacherInfo <- data.frame(ID,Title,course)
  row.names(TeacherInfo) <- AllTeacher
  for(i in 1:nrow(CourseInfo)){
    TeacherInfo[CourseInfo[i,"teacher"],"course"] <- paste(TeacherInfo[CourseInfo[i,"teacher"],"course"],row.names(CourseInfo)[i],sep = " ")
  }
  write.csv(TeacherInfo,"TeacherInfo.csv")
}

#search the course-choosing information of a student
Search_StudentChoosing <- function(ID){
  StdClsChos <- read.csv("StudentClassChoose.csv",header = T,row.names = 1)
  colnames(StdClsChos)[StdClsChos[which(StdClsChos[,"studentID"]==ID),]==1]
}

#search which students chose a specific course
Search_CourseChoosing <- function(course){
  StdClsChos <- read.csv("StudentClassChoose.csv",header = T,row.names = 1)
  rownames(StdClsChos)[StdClsChos[course]==1]
}

#modify the grades of a specific student
ModifyGrades <- function(ID,course,grades){
  CourseGrades <- read.csv("CourseGrades.csv",header = T,row.names = 1)
  n <- which(CourseGrades$studentID==ID)
  if(is.na(CourseGrades[n,course])){
    print("he/she hasn't chosen this course")
  }else{
    CourseGrades[n,course]=grades
    write.csv(CourseGrades,"CourseGrades.csv")
  }
}

#drop out a course for a specific student
DropOut <- function(ID,course){
  StdClsChos <- read.csv("StudentClassChoose.csv",header = T,row.names = 1)
  CourseGrades <- read.csv("CourseGrades.csv",header = T,row.names = 1)
  n <- which(StdClsChos$studentID==ID)
  if(StdClsChos[n,course]==0){
    print("he/she hasn't chosen this course")
  }else{
    StdClsChos[n,course] <- 0
    CourseGrades[n,course] <- NA
  }
  write.csv(StdClsChos,"StudentClassChoose.csv")
  write.csv(CourseGrades,"CourseGrades.csv")
}

#to select a specific course
SelectCourse <- function(ID,course,op="continue"){
  StdClsChos <- read.csv("StudentClassChoose.csv",header = T,row.names = 1)
  CourseGrades <- read.csv("CourseGrades.csv",header = T,row.names = 1)
  n <- which(StdClsChos$studentID==ID)
  if(StdClsChos[n,course]==1){
    print("he/she has already chosen this course")
  }else{
    StdClsChos[n,course] <- 1
    CourseGrades[n,course] <- sample(0:100,1)
  }
  if(op=="end"){
    CourseInfo <- read.csv("CourseInfo.csv",header = T,row.names = 1)
    TotalCredit <- 0
    for(i in 2:ncol(StdClsChos)){
      if(StdClsChos[n,i]==1)TotalCredit <- TotalCredit+CourseInfo[colnames(StdClsChos)[i],"credit"]
    }
    if(TotalCredit<20){
      write.csv(StdClsChos,"StudentClassChoose.csv")
      write.csv(CourseGrades,"CourseGrades.csv")
      print("the total credits of your selected courses still less than 20!!!please input an another course and your op")
      course <- readline()
      op <- readline()
      SelectCourse(ID,course,op)
    }else{
      write.csv(StdClsChos,"StudentClassChoose.csv")
      write.csv(CourseGrades,"CourseGrades.csv")
    }
  }
  else{
    write.csv(StdClsChos,"StudentClassChoose.csv")
    write.csv(CourseGrades,"CourseGrades.csv")
    print("please input an another course and your op")
    course <- readline()
    op <- readline()
    SelectCourse(ID,course,op)
  }
}
