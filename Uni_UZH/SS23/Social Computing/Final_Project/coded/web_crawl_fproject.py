import requests 
from bs4 import BeautifulSoup
from selenium import webdriver
import time

"""
	crawl the web with requests Selenium and bs4 
	extract the specific data needed gender based price discrimination 
	1. blank 
	2. female 
	3. male 

"""


url = 'https://www.amazon.com'

driver.get(url)
def get_url (search_term) :
	"""Generate a url from search term"""
	template = 'https://www.amazon.com/s?k={}&ref=nb_sb_noss_1'
	search_term = search_term.replace('+')
	return template.format(search_term)
	
	url = get_url('ultrawide monitor')
	print(url)
	#https://www.amazon.com/s?k=ultrawide+monitor&ref=nb_sb_noss_1
	driver.get (url)

# def crawl:

# 	browser = webdriver.Firefox(".../chromedriver")

# 	browser.get("https://www.amazon.de/ref=nav_logo")
# 	#https://www.instagram.com 
# 	#https://www.facebook.com
# 	#https://www.wish.com
# 	username = "..."
# 	password = ".."

# 	time.sleep(3)

# 	#log in
# 	browser.find_element_by_id("user_email").send_keys(username)
# 	browser.find_element_by_id("user_password").send_keys(password)

# 	#click
# 	browser.find_element_by_class_name("actions").click()

# 	#this allows the site to load
# 	time.sleep(5)

# 	#scrolls down
# 	browser.execute_script("window.scrollTo(0, document.body.scrollHeight);") 

# 	#saves page
# 	with open('gab_page.html', 'w') as f:
# 	    f.write(browser.page_source)

