source('make_question_item.R')

qti_build<-function(quiz,folder) {
  

# %
# % folder points to unzipped quiz folder
# %
# % quiz has:
#   % quiz.title
# % quiz.questions
# %
# % questions has:
#   % questions.question
# % questions.answers
# % questions.linkedFiles

quiz_title<-quiz$title
questions<-quiz$questions
n_questions<-nrow(questions)

quiz_ident<-quiz_title

group_questions_start=c(
  paste0('      <section ident="', quiz_ident, 'G" title="GroupQuestions', quiz_ident, '" >'),
  '        <selection_ordering>',
  '          <selection>',
  paste0('            <selection_number>', format(quiz$questions2Answer), '</selection_number>'),
  '            <selection_extension>',
  '              <points_per_item>1.0</points_per_item>',
  '            </selection_extension>',
  '          </selection>',
  '        </selection_ordering>'
)

group_questions_end='      </section>'

assessment_start=c(
  paste0('  <assessment ident="', quiz_ident, '" title="', quiz_title, '">'),
  '    <qtimetadata>',
  '      <qtimetadatafield>',
  '        <fieldlabel>cc_maxattempts</fieldlabel>',
  '        <fieldentry>1</fieldentry>',
  '      </qtimetadatafield>',
  '    </qtimetadata>',
  '    <section ident="root_section">'
)
assessment_end=c(
  '</section>',
  '  </assessment>'
)

group_questions_body=c()
for (qi in 1:n_questions){
  item_ident=paste0('g', format(qi))
  qti_item=make_question_item(questions[qi,],qi,item_ident)
  group_questions_body=c(group_questions_body,qti_item)
}

assessment=c(assessment_start,
            group_questions_start,
            group_questions_body,
            group_questions_end,
            assessment_end
)

qti=c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<questestinterop xmlns="http://www.imsglobal.org/xsd/ims_qtiasiv1p2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/ims_qtiasiv1p2 http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd">',
   assessment,
  '</questestinterop>'
)
print('Quiz assembled')

fileName=paste0(folder, filesep, quiz_ident, filesep, quiz_ident, '.xml')

append<-FALSE
for (f1 in 1:length(qti)) {
  fprintf(paste0(qti[f1], "\n"),file=fileName,append=append)
  append<-TRUE
}

print('Quiz written to file')

# %% meta file

if (is.element(question$type,c("numerical_question","fill_in_multiple_blanks_question"))) {
  description<-c(
'    <div>',
'      <p>All answers can be rounded to 3 significant figures, like these:</p>',
'      <p>123, 12.3, 1.23, 0.123, 0.0123, 0.00123</p>',
'    </div>'
  )
} else {
  description<-c()
}
qti_meta=c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  paste0('<quiz identifier="', quiz_ident, '" xmlns="http://canvas.instructure.com/xsd/cccv1p0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://canvas.instructure.com/xsd/cccv1p0 https://canvas.instructure.com/xsd/cccv1p0.xsd">'),
  paste0('  <title>', quiz_ident, '</title>'),
  paste0('  <description>', description, '</description>'),
  '  <shuffle_answers>true</shuffle_answers>',
  '  <scoring_policy>keep_highest</scoring_policy>',
  '  <hide_results></hide_results>',
  '  <quiz_type>assignment</quiz_type>',
  '  <show_correct_answers>true</show_correct_answers>',
  '  <one_question_at_a_time>true</one_question_at_a_time>',
  '  <cant_go_back>false</cant_go_back>',
  '  <one_time_results>false</one_time_results>',
  '  <show_correct_answers_last_attempt>false</show_correct_answers_last_attempt>',
  '</quiz>'
)
fileName=paste0(folder, filesep, quiz_ident, filesep, 'assessment_meta.xml')

append<-FALSE
for (f1 in 1:length(qti_meta)) {
  fprintf(qti_meta[f1],file=fileName,append=append)
  if (f1<length(qti_meta)) fprintf("\n",file=fileName,append=TRUE)
  append<-TRUE
}
print('Meta written to file')


# %% manifest 
links=questions$linkedFiles
files<-c()
for (i in 1:nrow(questions)) {
  files<-c(files,paste0('<file href="', questions[i,]$linkedFiles, '"/>'))
}

manifest=c(
'<?xml version="1.0" encoding="utf-8"?>',
paste0('<manifest identifier="MANIFEST-', quiz_ident, '" xmlns="http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1" xmlns:imsmd="http://www.imsglobal.org/xsd/imsmd_v1p2" xmlns:lom="http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsccv1p1/imscp_v1p1 http://www.imsglobal.org/xsd/imscp_v1p1.xsd http://ltsc.ieee.org/xsd/imsccv1p1/LOM/resource http://www.imsglobal.org/profile/cc/ccv1p1/LOM/ccv1p1_lomresource_v1p0.xsd http://www.imsglobal.org/xsd/imsmd_v1p2 http://www.imsglobal.org/xsd/imsmd_v1p2p2.xsd">'),
'	<metadata>',
'		<schema>IMS Content</schema>',
'		<schemaversion>1.1.3</schemaversion>',
'		<imsmd:lom>',
'			<imsmd:general>',
'				<imsmd:title>',
'					<imsmd:langstring xml:lang="en-US"/>',
'				</imsmd:title>',
'			</imsmd:general>',
'		</imsmd:lom>',
'	</metadata>',
'	<organizations/>',
'	<resources>',
paste0('		<resource href="', quiz_ident, filesep, quiz_ident, '.xml" identifier="', quiz_ident, '" type="imsqti_xmlv1p2">'),
paste0('			<file href="', quiz_ident, filesep, quiz_ident, '.xml"/>'),
'      <dependency identifierref="RESOURCE2"/>',
'		</resource>',
paste0('       <resource href="', quiz_ident, filesep, 'assessment_meta.xml" identifier="RESOURCE2" type="associatedcontent/imscc_xmlv1p1/learning-application-resource">'),
paste0('           <file href="', quiz_ident, filesep, 'assessment_meta.xml"/>'),
'		</resource>',
'       <resource>',
files,
'		</resource>',
'	</resources>',
'</manifest>'
)

fileName=paste0(folder, filesep, 'imsmanifest.xml')

append<-FALSE
for (f1 in 1:length(manifest)) {
  fprintf(paste0(manifest[f1],"\n"),file=fileName,append=append)
  append<-TRUE
}
print('Manifest written to file')

system(paste0('zip -r -q ', folder, '.zip ', folder, '/'))

print('Quiz zipped and ready to import')

}


