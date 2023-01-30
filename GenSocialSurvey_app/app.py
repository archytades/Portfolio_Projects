import numpy as np
import pandas as pd
import plotly.graph_objects as go
import plotly.express as px
import plotly.graph_objects as go
import plotly.figure_factory as ff
import dash
from dash import dcc
from dash import html
from dash.dependencies import Input, Output
external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']


#%%capture
gss = pd.read_csv("https://github.com/jkropko/DS-6001/raw/master/localdata/gss2018.csv",
                 encoding='cp1252', na_values=['IAP','IAP,DK,NA,uncodeable', 'NOT SURE',
                                               'DK', 'IAP, DK, NA, uncodeable', '.a', "CAN'T CHOOSE"], low_memory=False)
mycols = ['id', 'wtss', 'sex', 'educ', 'region', 'age', 'coninc',
          'prestg10', 'mapres10', 'papres10', 'sei10', 'satjob',
          'fechld', 'fefam', 'fepol', 'fepresch', 'meovrwrk'] 
gss_clean = gss[mycols]
gss_clean = gss_clean.rename({'wtss':'weight', 
                              'educ':'education', 
                              'coninc':'income', 
                              'prestg10':'job_prestige',
                              'mapres10':'mother_job_prestige', 
                              'papres10':'father_job_prestige', 
                              'sei10':'socioeconomic_index', 
                              'fechld':'relationship', 
                              'fefam':'male_breadwinner', 
                              'fehire':'hire_women', 
                              'fejobaff':'preference_hire_women', 
                              'fepol':'men_bettersuited', 
                              'fepresch':'child_suffer',
                              'meovrwrk':'men_overwork'},axis=1)
gss_clean.age = gss_clean.age.replace({'89 or older':'89'})
gss_clean.age = gss_clean.age.astype('float')




markdown2='''The **General Social Survey**, or GSS, in use since the 1970s collects data about modern society, including attitudes about societal trends and opinions. The survey primarily uses *mailed surveys*. According the the GSS "The GSS sample consists of a random sample of households and persons that are representative of households and people in the United States." It attempts to confirm these groupings through cross-validation with **census results**. The GSS outlines its methodology in [this document](https://gss.norc.org/Documents/reports/methodological-reports/MR134%20-%20Ballot%20and%20Form.pdf).'''


questions={"A working mother can establish just as warm and secure a relationship \nwith her children as a mother who does not work.":'relationship',
            "It is much better for everyone involved \nif the man is the achiever outside the home and \nthe woman takes care of the home and family.":'male_breadwinner',
            "Most men are better suited emotionally for politics than are most women.":'men_bettersuited',
            "A preschool child is likely to suffer if his or her mother works.":'child_suffer',
            "Family life often suffers because men concentrate too much on their work.":'men_overwork'}
questions = dict([(value, key) for key, value in questions.items()])


app=dash.Dash(__name__, external_stylesheets=external_stylesheets)
#apparently heroku needs this
server=app.server
app.layout = html.Div([ 
    html.H1("Insights from the GSS"),
    html.H3("by: Brandtly Jones"),
    html.H3("with huge appreciation to Prof. Jonathan Kropko"),
    html.Div([dcc.Markdown(children = markdown2)]),
    html.H3("Use the dropdown menus to compare responses to survey questions from people of different sexes, regions, and years of education."),
    html.Div([
            
            #the dropdown menus go here
            html.H3("Question"),
            #dropdown menu1
            dcc.Dropdown(id='cat_choice',
                options=[{'label': i, 'value': i} for i in ['relationship','male_breadwinner','men_bettersuited','child_suffer','men_overwork']],
                value='male_breadwinner'),
        
        html.H3("group"),
        #dropdown menu2
        dcc.Dropdown(id='group',
                     options=[{'label':i, 'value':i} for i in ['sex','region','education']],
                     value='sex')
                              
        
        ], style={'width': '25%', 'float': 'left'}),
        
        html.Div([
            #bar graph with input from dropdowns
            dcc.Graph(id="bargraph")
        
        ], style={'width': '70%', 'float': 'right'})

    
    
])    


@app.callback(Output(component_id='bargraph', component_property='figure'),
              [Input(component_id='cat_choice', component_property='value'),
               Input(component_id='group', component_property='value')])


def cat_choice(category, group):
    gss_clean[f'{category}']=gss_clean[f'{category}'].astype('category')

    responses = pd.crosstab(gss_clean[f'{group}'], gss_clean[f'{category}'])

    responses = responses[['strongly disagree', 'disagree','agree','strongly agree']].reset_index()

    responses = pd.melt(responses, id_vars = f'{group}', value_vars = ['strongly disagree', 'disagree','agree','strongly agree'])

    bar_fig = px.bar(responses, x=f'{category}', y='value', color=f'{group}',
                    
            labels={f'{category}':'Opinion', 'value':'Count'},
            title=questions[f'{category}'],
                     
            hover_data = [f'{group}', f'{category}'],
            barmode = 'group') 
    return bar_fig
    
if __name__ == '__main__':
    app.run_server(debug=True)

