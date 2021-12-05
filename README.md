# Faculty_Analyzer

# Project Goal
This project aims to create a computer-based tool with R language that matches a resume to program course description to discover gaps and recommend courses. 

# Project Description
The tool allows the user to upload their resume, select the study program, and identify courses already taken and courses needed for a selected concentration. The tool will score the resume against the syllabi and recommend courses to remove the gap between what students know and what they want to know. It can help the faculty member discover the gaps in their course covers to improve the course and help students discover the best match courses.


# Use Case
Users can visit the webpage to upload their resume as a source firstly. 
The resume should be a txt file. If users want to discover the most suitable recommended courses according to personal situations, they should also upload the course description CSV file as the target file. Then users can click Compute button, and the tool will automatically score the course match. Users should patiently wait for few seconds, and the result will display on their screen. 
According to the result, users can compare the similarity score to choose the most suitable course. If users want to save the result, they can press the "Download Socring Result" button at the end of the result to download it.

This system has four main functions:
1.	Upload resume txt file by users
2.	Upload course description CSV file by users
3.	Click the "Compute" button, the result will show in the tight side
4.	After the result scored,  click the "Download Socring Result" button to download a copy of the ranked list
