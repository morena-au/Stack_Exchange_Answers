# LanguageTool: Grammar And Spell Checker In Python
# https://pypi.org/project/language-tool-python/
# https://predictivehacks.com/languagetool-grammar-and-spell-checker-in-python/

import os
import pandas as pd
import language_tool_python
#tool = language_tool_python.LanguageToolPublicAPI('en')
tool = language_tool_python.LanguageTool('en-US')
# is_bad_rule = lambda rule: rule.message == 'Possible spelling mistake found.' and len(rule.replacements) and rule.replacements[0][0].isupper()

os.chdir("C:\\Projects\\Stack_Exchange\\01_motivation_feedback\\Answers\\data\\raw\\")
text = pd.read_csv("text.csv")
text = text[text['WithoutHTML'].notna()]

# for all the text collect the errors info and the ids
# write down in a csv file

# Initate an empty list 
Id = []
ruleId = []
message = []
replacements = []
offset = []
errorLength = []
sentece = []
category = []
ruleIssueType = []

for x in range(text.shape[0]): # 
    WithoutHTML = text.iloc[x, 1]
    PostId = text.iloc[x, 0]

    # run the LanguageTool check
    matches = tool.check(WithoutHTML)

    # for each issue/match found append it to the list
    for i in range(len(matches)):
        Id.append(PostId)
        ruleId.append(matches[i].ruleId)
        message.append(matches[i].message)
        replacements.append(matches[i].replacements)
        offset.append(matches[i].offset)
        errorLength.append(matches[i].errorLength)
        sentece.append(matches[i].sentence)
        category.append(matches[i].category)
        ruleIssueType.append(matches[i].ruleIssueType)

# write down the dataframe
grammar_spelling_analysis = pd.DataFrame({
    "Id" : Id, 
    "ruleId" : ruleId, 
    "message" : message, 
    "replacements" : replacements, 
    "offset" : offset, 
    "errorLength" : errorLength, 
    "sentece" : sentece, 
    "category" : category, 
    "ruleIssueType" : ruleIssueType})

# write a csv file
grammar_spelling_analysis.to_csv("grammar_spelling_analysis.csv", index=False)


