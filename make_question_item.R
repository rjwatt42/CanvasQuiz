make_question_item<-function(question,qi,item_ident) {
  
  n_answers<-nrow(question$answers)
  n_choices_per_answer=ncol(question$answers)
  answer_idents=1000+qi*10+(1:(n_choices_per_answer*n_answers))
  keywords=question$keywords
  answer_correct_margin<-c()
  
  for (i in 1:length(question$text)) {
    question$text[i]<-paste0('<p>', question$text[i], '</p>')
  }
  
nc=1
answers_qti=c()
answer_correct_idents<-c()
answer_correct_margin<-c()
for (j in 1:n_answers){
  switch(question$type,
         "multiple_dropdowns_question"={answer_correct_idents[j]=answer_idents[nc]},
         "multiple_choice_question"={answer_correct_idents[j]=answer_idents[nc]},
         "fill_in_multiple_blanks_question"={answer_correct_idents[j]=question$answers[j,1]},
         "single_numerical_question"={
           answer_correct_idents[j]=question$answers[j,1]
           answer_correct_margin[j]<-question$errorMargin[j]
           }
  )

  if (question$type=="single_numerical_question") {
    this_answer_qti=c(paste0('<response_str ident="response_', keywords[j], '" rcardinality="Single">'),
                      '<render_fib fibtype="Decimal">',
                      paste0('<response_label ident="', format(answer_idents[nc]), '">'),
                      '</response_label>',
                      '</render_fib>',
                      '</response_str>'
    )
    nc=nc+1
  } else {
    this_answer_qti=paste0('<response_lid ident="response_', keywords[j], '">')
    if (!is.null(keywords[j])) {
      this_answer_qti=c(this_answer_qti,
                        '<material>',
                        paste0('<mattext>', keywords[j], '</mattext>'),
                        '</material>'
      )
    }
    
    this_answer_qti=c(this_answer_qti,'<render_choice>')
    
    for (i in 1:n_choices_per_answer) {
      this_answer_qti=c(this_answer_qti,
                        paste0('<response_label ident="', format(answer_idents[nc]), '">'),
                        '<material>',
                        paste0('<mattext texttype="text/plain">', question$answers[j,i], '</mattext>'),
                        '</material>',
                        '</response_label>'
      )
      nc=nc+1
    }
    this_answer_qti=c(this_answer_qti,'</render_choice>')
    this_answer_qti=c(this_answer_qti,'</response_lid>')
  }
  answers_qti=c(answers_qti,this_answer_qti)
}

header=qti_header(qi,question$type,answer_idents,item_ident);
presentation=qti_presentation(question$text,answers_qti);
scoring=qti_scoring(question$type,100,0,keywords,answer_correct_idents,answer_correct_margin);

qti_item=c(
  paste0('      <item ident="', item_ident, 'a', format(qi), '" title="Q', format(qi), '">'),
  header,
  presentation,
  scoring,
  '      </item>'
)
}

qti_presentation<-function(question_qti,answers_qti) {
  presentation=c(
    '<presentation>',
    '<material>',
    '<mattext texttype="text/html">',
    '<div>',
    question_qti,
    '</div>',
    '</mattext>',
    '</material>',
    answers_qti,
    '</presentation>'
  )
}

qti_scoring<-function(qtype,maxvalue,minvalue,keywords,answer_correct_idents,answer_correct_margin) {
  scoring=c(
    '        <resprocessing>',
    '          <outcomes>',
    paste0('            <decvar maxvalue="', maxvalue, '" minvalue="', minvalue, '", varname="SCORE", vartype="Decimal"/>'),
    '          </outcomes>'
  )
  for (j in 1:length(keywords)) {
    if (is.element(qtype,c("multiple_dropdowns_question","multiple_choice_question","fill_in_multiple_blanks_question"))) {
      responseCheck<-paste0('<varequal respident="response_', keywords[j], '">', 
                            format(answer_correct_idents[j]), 
                            '</varequal>')
    } else {
      responseCheck<-c(
        paste0('<vargte respident="response_', keywords[j], '">', 
                            format(answer_correct_idents[j]+answer_correct_margin[j]), 
                            '</vargte>'
        ),
        paste0('<varlte respident="response_', keywords[j], '">', 
                             format(answer_correct_idents[j]-answer_correct_margin[j]), 
                             '</varlte>'
        )
      )
    }
    
    scoring=c(scoring,
              '          <respcondition continue="No">',
              '            <conditionvar>',
              responseCheck,
              '            </conditionvar>',
              '            <setvar action="Add" varname="SCORE">1</setvar>',
              '          </respcondition>'
    )
  }
  scoring=c(scoring,'        </resprocessing>')
}

qti_header<-function(qi,qtype,answer_idents,item_ident) {
  if (qtype=="single_numerial_question") qtype<-"numerial_question"
  header=c(
    '        <itemmetadata>',
    '          <qtimetadata>',
    '            <qtimetadatafield>',
    '              <fieldlabel>question_type</fieldlabel>',
    paste0('              <fieldentry>', qtype, '</fieldentry>'),
    '            </qtimetadatafield>',
    '            <qtimetadatafield>',
    '              <fieldlabel>points_possible</fieldlabel>',
    '              <fieldentry>1.0</fieldentry>',
    '            </qtimetadatafield>',
    '            <qtimetadatafield>',
    '              <fieldlabel>original_answer_ids</fieldlabel>',
    paste0('              <fieldentry>', gsub('  ',',',format(answer_idents)), '</fieldentry>'),
    '            </qtimetadatafield>',
    '            <qtimetadatafield>',
    '              <fieldlabel>assessment_question_identifierref</fieldlabel>',
    paste0('              <fieldentry>', item_ident, 'b', format(qi), '</fieldentry>'),
    '            </qtimetadatafield>',
    '          </qtimetadata>',
    '        </itemmetadata>'
  )
}
