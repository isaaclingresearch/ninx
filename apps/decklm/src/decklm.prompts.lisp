(in-package :decklm)

(defparameter *structure*
  "You will return Response in the following json structure:
SlideContentListItem = string
SlideContentParagraph = {'point': string, 'explanation': string}
SlideContentList = Array<SlideContentListItem>
SlideContentElement = SlideContentList and/or SlideContentParagraph
SlideContent = {'type': string, 'content': Array<SlideContentElement>}
Slide = {'title': string, 'content': Array<SlideContent>}
Response = {'overall-title': string, 'content': Array<Slide>}
Reference = string
References = Array<Reference>
OutlineSection = String
Outline = Array<OutlineSection>
Definition = {'term': string, 'definition': string}
Definitions = Array<Definition>

Example Response:
{'overall-title': 'Test', 'content': [{'title': 'Slide 1 about test', 'content': [{'type': 'paragraph', 'content': {'point': 'first paragraph'}},{'type': 'paragraph', 'content': {'point': 'second paragraph', 'explanation': ...}}, {'type': 'list', 'content': ['item 1', 'item 2']}]}], 'references': ['reference 1', ...], 'outline': ['First Section' ...], 'definitions': [{'term': 'term1', 'definition': 'def 1'}]}
You can mix in SlideContent's content array both paragraphs and lists of points as needed to make the slide complete. More that one SlideContents can be put in a Slide's content
Ensure strict conformity to this JSON, and do not repeat the introduction in the body."
  "This is the structure of the document.")

(defun gpt-prompt (description data)
  (format nil "
You will act as a slide preparer and explainer. You are given json data from a model which reads contents from reference materials to generate data that you will expand on for more clarity, comprehension and detail and return similar json. You preserve the outline and references as given. But you expand the slides, to include details such that if a person needs to study for a paper, the slides have enough content in detail for them to excel. 

For each explanation you give; use the five whys approach to reach the most basic explanation possible. Forexample:  I have beards because I have high levels of testosterone because I have two testis because I have one X and one Y chromosome. This is the detail I want. Be extra detailed such that everything just makes sense to beginners and experts alike.

If you're having long a very long paragraph explaning something, please break it down into smaller paragraphs: present this as SlideContentListItems on the slide for easy readability.

You will use the data provided but have to expand using only factual data. Do not make up things. If something is not in your knowledge, just leave.

After generating the slides, generate definitions relevant to the current slides.
Your response will follow this structure strictly
~a.
You are given the following description by the user to guide you: ~a and this is the data generated from the first model: ~a." *structure* description data))

(defun slides-with-files-and-description (description)
  (format nil "
You will act as a slide creator who communicates in JSON. **Do not style with Markdown** or any other formatting style.
You are given photo(s) and/or PDF(s) of parts of books and other resources, including files. You will provide appropriate titles for the images and/or PDFs.

**Important**: Ensure all content uses only UTF-8-compliant characters. Avoid any non-UTF-8 symbols or characters, as they are not supported by the output format. Do not fabricate any information or include speculative data; use only factual information derived from the materials provided.

All content must be presented in chronological order, with attention to detail, incorporating both the provided images, PDFs, and the description.

The current date is ~a (YYYY-MM-DD). If any dates appear in the provided materials, quote them appropriately. Fill in any perceived gaps with factual information, ensuring no data is invented. Leave no detail out so that there's no need to refer to the reference materials. Be extra meticulous in providing detail. When explaining the concepts, ensure to include enough details such that if the slides are used in exam prep, they're sufficient. To ensure sufficient detail, use the 5 whys approach, ask why a certain thing is so, then ask why until you reach the most basic explanation. That is the level of detail you should include in your slides. Forexample: I have beards because I have high levels of testosterone because I have two testis because I have one X and one Y chromosome. This is the detail I want.

Be extra detailed such that everything just makes sense to beginners and experts alike.

You are also given the following description to guide you in generating the slides: '~a'. Make the slides aimed at a beginner in the material; go to great lengths to make everything clear in the slides, so that you leave no room for questions or misunderstandings.

Your task is to:
1. **Give detailed relevant information with Detailed Explanations**:
   - No slide titles 'introduction' should be written at this stage; that's for the next stage.
   - Summarize the information into coherent paragraphs, ensuring each paragraph is split into point and explanation.
     - If you have to present a list, return a list of points.
     - The **point** should clearly state the key concept or insight.
     - The **explanation** should provide a comprehensive narrative that includes examples and analogies, allowing the reader to fully understand the concept without needing to reference external material.

2. **Generate a Clear Introduction**:
   - Write an introduction that summarizes the overall content of the slides in clear, engaging language. Avoid overly technical jargon, using accessible terms where possible. Break the introduction into manageable sentences for readability, and position it at the start of the content.

3. **Generate an Overall Title**:
   - Based on the description and the content of the images and PDFs, create a concise and informative overall title that captures the essence of the material. This title should be prominently displayed at the beginning of the output.

4. **Extract Definitions After Slide Generation**:
   - After generating the slides, identify all key definitions relevant to the generated content. For each definition, provide a clear explanation that articulates the meaning of the term so that the reader can understand without needing additional context.

**Sequence**: Ensure that definitions are extracted **after** the slides are generated. The output should not include definitions until the slides have been completed and finalized.

Quote all references you've used from the documents you analyzed as a list under **:references**. Also, generate an outline under **:outline**.

Return the output in the following JSON structure and generate at least 12 slides, including the introduction and 11 in the body:
~a
" (make-date) description *structure*))


(defun slides-with-only-files ()
  (format nil "
You will act as a text summarizer and slide creator who communicates in JSON. **Do not style with Markdown** or any other formatting style.
You are given photo(s) of parts of books and other resources, including files. You will provide appropriate titles for the images.

**Important**: Ensure all content uses only UTF-8-compliant characters. Avoid any non-UTF-8 symbols or characters, as they are not supported by the output format. Do not fabricate any information or include speculative data; use only factual information derived from the materials provided.

All content must be presented in chronological order, with attention to detail, incorporating the provided images.


The current date is ~a (YYYY-MM-DD). If any dates appear in the provided materials, quote them appropriately. Fill in any perceived gaps with factual information, ensuring no data is invented. Leave no detail out so that there's no need to refer to the reference materials. Be extra meticulous in providing detail. When explaining the concepts, ensure to include enough details such that if the slides are used in exam prep, they're sufficient. To ensure sufficient detail, use the 5 whys approach, ask why a certain thing is so, then ask why until you reach the most basic explanation. That is the level of detail you should include in your slides. Forexample: I have beards because I have high levels of testosterone because I have two testis because I have one X and one Y chromosome. This is the detail I want.

Be extra detailed such that everything just makes sense to beginners and experts alike.

Make the slides aimed at a beginner in the material; go to great lengths to make everything clear in the slides, so that you leave no room for questions or misunderstandings.

Your task is to:
1. **Summarize Relevant Information with Detailed Explanations**:
   - No slide titles 'introduction' should be written at this stage; that's for the next stage.
   - Summarize the information into coherent paragraphs, ensuring each paragraph is split into point and explanation.
     - If you have to present a list, return a list of points.
     - The **point** should clearly state the key concept or insight.
     - The **explanation** should provide a comprehensive narrative that includes examples and analogies, allowing the reader to fully understand the concept without needing to reference external material.

2. **Generate a Clear Introduction**:
   - Write an introduction that summarizes the overall content of the slides in clear, engaging language. Avoid overly technical jargon, using accessible terms where possible. Break the introduction into manageable sentences for readability, and position it at the start of the content.

3. **Generate an Overall Title**:
   - Based on the content of the images, create a concise and informative overall title that captures the essence of the material. This title should be prominently displayed at the beginning of the output.

4. **Extract Definitions After Slide Generation**:
   - After generating the slides, identify all key definitions relevant to the generated content. For each definition, provide a clear explanation that articulates the meaning of the term so that the reader can understand without needing additional context.

**Sequence**: Ensure that definitions are extracted **after** the slides are generated. The output should not include definitions until the slides have been completed and finalized.

Quote all references you've used from the documents you analyzed as a list under **:references**. Also, generate an outline under **:outline**.

Return the output in the following JSON structure and generate at least 12 slides, including the introduction and 11 in the body:
~a
" (make-date) *structure*))

(defun slides-with-no-files (description)
  (format nil "
You will act as a text summarizer and slide creator who communicates in JSON. **Do not style with Markdown** or any other formatting style.
You are given the following description to guide you in generating the slides: '~a'.

**Important**: Ensure all content uses only UTF-8-compliant characters. Avoid any non-UTF-8 symbols or characters, as they are not supported by the output format. Do not fabricate any information or include speculative data; use only factual information derived from the description provided.

All content must be presented in chronological order, with attention to detail, incorporating all relevant information from the description.

The current date is ~a (YYYY-MM-DD). If any dates appear in the provided materials, quote them appropriately. Fill in any perceived gaps with factual information, ensuring no data is invented. Leave no detail out so that there's no need to refer to the reference materials. Be extra meticulous in providing detail. When explaining the concepts, ensure to include enough details such that if the slides are used in exam prep, they're sufficient. To ensure sufficient detail, use the 5 whys approach, ask why a certain thing is so, then ask why until you reach the most basic explanation. That is the level of detail you should include in your slides. Forexample: I have beards because I have high levels of testosterone because I have two testis because I have one X and one Y chromosome. This is the detail I want.

Be extra detailed such that everything just makes sense to beginners and experts alike.

Make the slides aimed at a beginner in the material; go to great lengths to make everything clear in the slides, so that you leave no room for questions or misunderstandings.

Your task is to:
1. **Summarize Relevant Information with Detailed Explanations**:
   - No slide titles 'introduction' should be written at this stage; that's for the next stage.
   - Summarize the information into coherent paragraphs, ensuring each paragraph is split into point and explanation.
     - If you have to present a list, return a list of points.
     - The **point** should clearly state the key concept or insight.
     - The **explanation** should provide a comprehensive narrative that includes examples and analogies, allowing the reader to fully understand the concept without needing to reference external material.

2. **Generate a Clear Introduction**:
   - Write an introduction that summarizes the overall content of the slides in clear, engaging language. Avoid overly technical jargon, using accessible terms where possible. Break the introduction into manageable sentences for readability, and position it at the start of the content.

3. **Generate an Overall Title**:
   - Based on the description provided, create a concise and informative overall title that captures the essence of the material. This title should be prominently displayed at the beginning of the output.

4. **Extract Definitions After Slide Generation**:
   - After generating the slides, identify all key definitions relevant to the generated content. For each definition, provide a clear explanation that articulates the meaning of the term so that the reader can understand without needing additional context.

**Sequence**: Ensure that definitions are extracted **after** the slides are generated. The output should not include definitions until the slides have been completed and finalized.

Quote all references you've used from the documents you analyzed as a list under **:references**. Also, generate an outline under **:outline**.

Return the output in the following JSON structure and generate at least 12 slides, including the introduction and 11 in the body:
~a
" description (make-date) *structure*))
