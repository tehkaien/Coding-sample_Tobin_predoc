
import os, sys, glob, re, codecs, shutil

sep_line = '=================================================================='

pdirname = '個人發言紀錄綜合總表'


#=========================================================================
#=========================================================================


def find_dirs():

    #========================================================
    #
    #   找出此路徑之下所有的目錄(資料夾)、列印目錄名稱、
    #
    #========================================================

    full_path = os.getcwd()  # 取得目前所在工作區的絕對路徑

    tmp = full_path.split('\\')
    
    dir_name = tmp[len(tmp)-1]   # 取得目前所在工作區的目錄名稱
    

    # 找出此工作路徑之下所有的目錄或資料夾
    # 
    dirs = [d for d in os.listdir(full_path) if os.path.isdir(os.path.join(full_path, d))]

    #print(dirs)

    # 在螢幕上列印出所有的目錄或資料夾名稱
    #
    print('\n' + sep_line + '\n')
    
    if len(dirs) == 0:

        print('在\"%s\"中未發現任何目錄或資料夾！' %(dir_name))

        return full_path, files  # 如未發現文字檔則回主程式

    else:

        print('在\u3010%s\u3011中包含%d個目錄與資料夾，分別為：\n' %(dir_name, len(dirs)))
    
        counter = 0     # 目錄計數器
    
        for d in dirs:

            counter += 1
        
            print(' (%02d). %s' %(counter, d))   # 列印目錄名稱

        print('\n' + sep_line + '\n\n')
        

    return full_path, dirs 



#=========================================================================
#=========================================================================


def find_files( full_path ):

    #========================================================
    #
    #   找出此路徑之下所有的文字檔(.txt)、列印檔案名稱、
    #
    #========================================================

    tmp = full_path.split('\\')
    
    dir_name = tmp[len(tmp)-1]   # 取得目前所在工作區的資料夾名稱


    # 找出此工作路徑之下所有的文字檔(.txt)
    # 
    files = [f for f in glob.glob("*.txt") if os.path.isfile(os.path.join(full_path, f))] 

    # 在螢幕上列印出所有文字檔的檔案名稱
    #
    if len(files) == 0:

        print('在\"%s\"中未發現文字檔案！' %(dir_name))

        return files  # 如未發現文字檔則回主程式

    else:

        print('在\u3010%s\u3011中包含%d個文字檔案，分別為：\n' %(dir_name, len(files)))
    
        counter = 0     # 檔案計數器
    
        for file in files:

            counter += 1
        
            print(' (%02d). %s' %(counter, file))   # 列印檔案名稱

    return files 



#=========================================================================
#=========================================================================


def file_process( p_path, full_path, files ):

    file_dir_list = []
    
    for file in files:

        file_dir_list.append(os.path.splitext(file)[0])

    #print('\n', file_dir_list)

    for fd in file_dir_list:

        try:
            
            # 產生資料夾，無論資料夾是否已經存在，一律重新產生
            os.makedirs(fd, exist_ok = True)

        except OSError:  # 生成資料夾錯誤例外

            print('\n[錯誤] \'%s\'資料夾生成失敗！' % (fd))
            
        else:

            print('\n\n>>> \u300A%s\u300B資料夾已生成，檔案進行分類處理中 ...' % (fd))

            dir_path = os.path.join(full_path, fd)      # 紀錄新生成檔案資料夾的絕對路徑

            file_path = os.path.join(full_path, fd + '.txt')    # 紀錄原始公報文字檔案的絕對路徑

            record_process(fd, p_path, dir_path, file_path)     # 搜尋立法委員的人數與每個委員的發言紀錄次數



#=========================================================================
#=========================================================================


def record_process( fd, p_path, dir_path, file_name):

    #print(fd, '\n', dir_path, '\n', file_name)

    #==============================================================
    #
    #           搜尋立法委員的人數與每個委員的發言紀錄次數
    #
    #==============================================================

    fin = codecs.open(file_name, 'rb', 'big5', 'replace')

    '''
    for line in fin:

        #line.encode('utf-8')

        #line.replace(b'\xbdI', '  ')
        print(line.encode('utf-8')) # this works if screen is utf-8

        print(line) # will this work fine, too?
    '''
    
    s = fin.read()

    #
    #   以下為 big5 碼無法轉換字元的暫時處理的方式
    #
    if s.find(' �I') != -1:

        s = s.replace(' �I', '')    # 直接移除字元 ' �I'
        
    if s.find('�I') != -1:

        s = s.replace('�I', '')     # 直接移除字元 '�I'

    fin.close()

    s = removeTitleAndPageNumber(s)     # 移除文件頁尾標題與頁碼
    
    #print(s)
    
    #========================================================

    #rule1 = '\w*.*\w*[(\b委員\b)(\b書面意見\b)]\w*：'
    
    rule1 = '[^？！…※‥。·，、；：）｝〕】》〉」＃〞〝”“’‘』╴—–｜ \n]+[(\b委員\b)(\b書面質詢\b)(\b書面意見\b)][\w[^(\b等\b)]\w]*：'
    
    names = set(re.findall(rule1, s))

    name_list = [w for w in names if ((w.find('委員') != -1) or (w.find('書面意見') != -1) or (w.find('書面質詢') != -1)) \
                 and (w.find('委員會') == -1) \
                 and (w.find('委員參考') == -1) \
                 and (w.find('委員在') == -1) \
                 and (w.find('委員提') == -1) \
                 and (w.find('出席委員') == -1) \
                 and (w.find('列席委員') == -1) \
                 and (w.find('主任委員') == -1) \
                 and (w.find('主住委員') == -1) \
                 and (w.find('主任秘書') == -1) \
                 and (w.find('請假委員') == -1) \
                 and (w.find('調解委員') == -1) \
                 and (w.find('提案') == -1) \
                 and (w.find('部門') == -1) \
                 and (w.find('剛才') == -1) \
                 and (w.find('問題') == -1) \
                 and (w.find('本席') == -1) \
                 and (w.find('要求') == -1) \
                 and (w.find('所以') == -1) \
                 and (w.find('各位') == -1) \
                 and (w.find('如下') == -1) \
                 and (w.find('意見') == -1) \
                 and (w.find('已經') == -1) \
                 and (w.find('因此') == -1) \
                 and (w.find('部分') == -1) \
                 and (w.find('國') == -1) \
                 and (w.find('嗎') == -1) \
                 and (w.find('說') == -1) \
                 and (w.find('的') == -1) \
                 and (w.find('或') == -1) \
                 and (w.find('是') == -1) \
                 and (w.find('所') == -1) \
                 and (w.find('第') == -1) \
                 and (w.find('我') == -1) \
                 and (w.find('你') == -1) \
                 and (w.find('他') == -1) \
                 and (w.find('能') == -1) \
                 and (w.find('次') == -1) \
                 and (w.find('不是') == -1) \
                 and (w.find('現在') == -1) \
                 and (w.find('之一') == -1) \
                 and (w.find('這麼做') == -1) \
                 and (w.find('這麼說') == -1) \
                 and (w.find('專門委員') == -1)]

    for i in range(0, len(name_list)):

        s1 = name_list[i]
        
        x = s1.find('、')

        if x != -1 and x <= 3:

           s2 =  s1[0:x+1]

           name_list[i] = s1.replace(s2, '')
      

    name_list.sort()

    number_of_names = len(name_list)    # 公報文件中的發言委員人數
                   
    #print(name_list, len(name_list))

    dic = {w : s.count(w) for w in name_list}

    #print(dic, len(dic))

    #==============================================================
    #
    #                       產生紀錄報表
    #
    #==============================================================

    doc_report = fd + '_report.txt'     # 紀錄報表的檔案名稱

    doc_report_path = os.path.join(dir_path, doc_report)    # 紀錄報表檔案的絕對路徑

    print('\n>> 於 %s 路徑下產生檔案。 ' % (dir_path ))
    
    print('\n> 正在產生報表：%s' % (doc_report))
    
    try:

        with open( doc_report_path, 'wt', encoding = 'utf8') as fout:

            print('\n%s 檔案中共有 %d 位委員的發言紀錄。\n\n' % ((fd + '.txt'), number_of_names), file = fout)

            counter = 0
            
            for m, n in dic.items():

                if (m.find('書面意見') == -1 and m.find('書面質詢') == -1):

                            # \u3000 是一個中文全形空白格，用於對齊中英文數字夾雜時的字串輸出之用。    
                    print(' {0:\u3000<6s} {1:s} {2:02d} {3:s}'.format( m[:-1], '的發言紀錄次數為',  n, '次。'), end = '\t', file = fout)

                elif (m.find('書面意見') != -1 or m.find('書面質詢') != -1):

                           # \u3000 是一個中文全形空白格，用於對齊中英文數字夾雜時的字串輸出之用。    
                    print(' {0:\u3000<6s} {1:s} {2:02d} {3:s}'.format( m[:-1], '的紀錄次數為',  n, '次。'), end = '\t', file = fout)

                else:

                    pass

                counter = counter + 1

                if counter == 2:

                    print('\n', file = fout)

                    counter = 0
                    
    except Exception as e:

        print('\n> [錯誤] 無法寫入\'%s\'檔案中！(Error：%s)\n' % (doc_report, e))

    else:

        fout.close()

        print('> 成功產生報表：%s' % (doc_report))
        
        
    #==============================================================
    #
    #                       產生委員個人報表
    #
    #==============================================================

    for pn in name_list:

        uname = pn[:-1].replace('委員', '')

        doc_precord = fd + '_' + uname + '.txt'     # 紀錄委員個人發言紀錄報表的檔案名稱

        doc_precord_path = os.path.join(dir_path, doc_precord)    # 紀錄委員個人發言紀錄報表檔案的絕對路徑

        print('\n> 正在產生紀錄檔：%s' % (doc_precord))

        s = s.replace(pn, '\n'+pn)

        rule3 = re.compile(r'%s[\w\W]+?[(\b？\b)(\b。\b)(\b！\b)(\b…\b)(\b‥\b)\s](?=\w*[ \
                  (\b委員\b)(\b主席\b))(\b署長\b)(\b部長\b)(\b處長\b)(\b廳長\b)(\b研析\b) \
                  (\b秘書\b)(\b次長\b)(\b專門委員\b)(\b組長\b)(\b科長\b)(\b科員\b) \
                  (\b總工程司\b)(\b股長\b)(\b簡任\b)(\b專員\b)(\b法官\b)(\b檢查\b)(\b視察\b) \
                  (\b司長\b)(\b技正\b)(\b主任\b)(\b管理師\b)(\b技士\b)(\b總長\b)(\b局長\b) \
                  (\b書記\b)(\b辦事\b)(\b設計\b)(\b助理\b)(\b人事長\b)(\b會計\b)(\b常務\b) \
                  (\b公使\b)(\b代表\b)(\b領事\b)(\b工程\b)(\b工務\b)(\b監工\b)(\b研究\b) \
                  (\b研議\b)(\b技監\b)(\b技士\b)(\b技佐\b)(\b技師\b)(\b管制\b)(\b督察\b) \
                  (\b氣象\b)(\b預報\b)(\b審查\b)(\b司法\b)(\b事務官\b)(\b法醫\b)(\b執行官\b) \
                  (\b典獄長\b)(\b所長\b)(\b院士\b)(\b館長\b)(\b分析師\b)(\b參事\b)(\b經理\b) \
                  (\b大使\b)(\b院長\b)(\b指揮官\b)(\b旅長\b)(\b艦隊長\b)(\b聯隊長\b)(\b上校\b) \
                  (\b政戰\b)(\b系主任\b)(\b教授\b)(\b主委\b)(\b司令\b)(\b參謀\b)(\b校長\b) \
                  (\b書面意見\b) \
                  ]\w*?：)' % (re.escape(pn)))
     
        try:

            with open( doc_precord_path, 'wt', encoding = 'utf8') as fout:

                recordList = rule3.findall(s)

                for msg in recordList:

                    msg = msg.replace(pn, '')

                    msg = msg.replace(r'（在席位上）', '')

                    msg = msg.replace(r'（在台下）', '')
        
                    print('\n'+ msg +'\n', file = fout)
                    
        except Exception as e:

            print('\n> [錯誤] 無法寫入\'%s\'檔案中！(Error：%s)\n' % (doc_precord, e))

        else:

            fout.close()

            print('> 成功產生紀錄檔：%s' % (doc_precord))


        pr_precord = uname + '.txt'

        pr_precord_path = os.path.join(p_path, pr_precord)

        try:

            with open( pr_precord_path, 'at', encoding = 'utf8') as fout:

                recordList = rule3.findall(s)

                for msg in recordList:

                    msg = msg.replace(pn, '')

                    msg = msg.replace(r'（在席位上）', '')

                    msg = msg.replace(r'（在台下）', '')
        
                    print('\n'+ msg +'\n', file = fout)
                    
        except Exception as e:

            print('\n> [錯誤] 無法寫入\'%s\'資料夾內的紀錄檔：%s！(Error：%s)\n' % (pdirname, pr_precord, e))

        else:

            fout.close()

            print('> 成功更新\'%s\'資料夾內的紀錄檔：%s' % (pdirname, pr_precord))        





#=========================================================================
#=========================================================================


def removeTitleAndPageNumber( file_contents ):

    #=======================================================
    #
    #               移除文件頁尾標題與頁碼
    #
    #=======================================================
    
    #rule1 = '立法院公報 第[\w\W]+?期 委員會紀錄\s+'

    rule1 = '[(\b立法院公報 第\b)][\w\W]+?[(\b期 \b)]\w+[(\b紀錄\b)]'

    title = re.match( rule1, file_contents )

    if title != None:
        
        rule2 = '\d{1,4}\s+[(\b立法院公報 第\b)][\w\W]+?[(\b期 \b)]\w+[(\b紀錄\b)]\s+'

        file_contents = re.sub( rule2, "", file_contents )

        #print('\n\n', file_contents)
    
    return file_contents


        
#=========================================================================
#=========================================================================


def main():

    cwd_path, dirs = find_dirs()    # 找出目前工作資料夾目錄的絕對路徑與此路徑下所有的資料夾(目錄)


    for dir_name in dirs:

        dir_name_path = os.path.join(cwd_path, dir_name)    # 將一個特定子目錄的絕對路徑找出來

        os.chdir(dir_name_path)     # 將工作區轉至該子目錄下

        pdir_name_path = os.path.join(dir_name_path, pdirname)
        
        if os.path.exists(pdir_name_path):

            try:

                shutil.rmtree(pdir_name_path)

            except OSError as e:

                print(e)

            else:

                print("\n舊的資料夾\"%s\"已經成功刪除！\n" % (pdirname))

        try:

            os.mkdir(pdir_name_path) 

        except OSError as e:

            print(e)

        else:

            print("\n新的資料夾\"%s\"已經成功建立！\n" % (pdirname))

        
        files = find_files(dir_name_path)       # 找出該子目錄下所有可能的文字檔(.txt)

        if files != []:

            file_process(pdir_name_path, dir_name_path, files)

        print('\n' + sep_line + '\n')


    return None



#=========================================================================
#=========================================================================    


if __name__ == '__main__':

    print( '\n... 啟動程式執行', end = '\n\n' )

    main()  # 主程序進入點

    print('\n程式執行完畢 ...\n')
 
