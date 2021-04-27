package socialmedia;

import java.io.*;
import java.util.*;

/**
 * BadSocialMedia is a minimally compiling, but non-functioning implementor of
 * the SocialMediaPlatform interface.
 * 
 * @author Diogo Pacheco
 * @version 1.0
 */
public class SocialMedia implements SocialMediaPlatform {

  private static final long serialVersionUID = -8546400894649906795L;
  private ArrayList<SocialMediaAccount> accounts = new ArrayList<SocialMediaAccount>();
  private ArrayList<SocialMediaPost> posts = new ArrayList<SocialMediaPost>();

  @Override
  public int createAccount(String handle) throws IllegalHandleException, InvalidHandleException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);
    
    if (account != null) {
      throw new IllegalHandleException();
    }
      
    account = new SocialMediaAccount(handle);
    accounts.add(account);
    return account.getId();

  }

  @Override
  public int createAccount(String handle, String description) throws IllegalHandleException, InvalidHandleException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);
    
    if (account != null) {
      throw new IllegalHandleException();
    }
    
    account = new SocialMediaAccount(handle, description);
    accounts.add(account);
    return account.getId();
  }

  @Override
  public void removeAccount(int id) throws AccountIDNotRecognisedException {
    SocialMediaAccount account = Helper.getAccount(id, accounts);

    if (account == null) {
      throw new AccountIDNotRecognisedException();
    }

    for (int i = 0; i < posts.size(); i++) {
      try {
        SocialMediaPost post = posts.get(i);
        if (post.getAccount() != null && post.getAccount().equals(account)) {
          deletePost(post.getId());
        }
      } catch (PostIDNotRecognisedException e) {
        e.printStackTrace();
      }
    }
    
    accounts.remove(account);
  }

  @Override
  public void removeAccount(String handle) throws HandleNotRecognisedException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);

    if (account == null) {
      throw new HandleNotRecognisedException();
    }

    accounts.remove(account);

  }

  @Override
  public void changeAccountHandle(String oldHandle, String newHandle) throws HandleNotRecognisedException, IllegalHandleException, InvalidHandleException {
    SocialMediaAccount account = Helper.getAccount(oldHandle, accounts);

    if (account == null) {
      throw new HandleNotRecognisedException();
    }

    account.setHandle(newHandle);
  }

  @Override
  public void updateAccountDescription(String handle, String description) throws HandleNotRecognisedException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);

    if (account == null) {
      throw new HandleNotRecognisedException();
    }

    account.setDescription(description);
  }

  @Override
  public String showAccount(String handle) throws HandleNotRecognisedException {
    StringBuilder builder = new StringBuilder();
    SocialMediaAccount account = Helper.getAccount(handle, accounts);

    if (account == null) {
      throw new HandleNotRecognisedException();
    }
      
    builder.append("ID: " + account.getId() + "\n");
    builder.append("Handle: " + account.getHandle() + "\n");
    builder.append("Description: " + account.getDescription() + "\n");
    builder.append("Post count: " + account.getPosts().size() + "\n");
    builder.append("Endorse count: " + account.getEndorsedPosts().size() + "\n");
    
    return builder.toString();
  }

  @Override
  public int createPost(String handle, String message) throws HandleNotRecognisedException, InvalidPostException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);

    if (account == null) {
      throw new HandleNotRecognisedException();
    }
    
    SocialMediaPost post = new SocialMediaPost(account, message);
    posts.add(post);
    account.getPosts().add(post);

    return post.getId();
  }

  @Override
  public int endorsePost(String handle, int id) throws HandleNotRecognisedException, PostIDNotRecognisedException, NotActionablePostException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);
    SocialMediaPost post = Helper.getPost(id, posts);
    SocialMediaPost endorsedPost = null;

    if (account == null) {
      throw new HandleNotRecognisedException();
    }

    if (post == null) {
      throw new PostIDNotRecognisedException();
    }

    if (Boolean.TRUE.equals(post.getEndorsed())) {
      throw new NotActionablePostException();
    }

    for (SocialMediaPost endorsement:post.getEndorsements()) {
      if (endorsement.getAccount() == account) {
        throw new NotActionablePostException();
      }
    }

    String message = "EP@" + account.getHandle() + ": " + post.getMessage();
    try {
      endorsedPost = new SocialMediaPost(account, message);
    } catch (InvalidPostException e) {
      e.printStackTrace();
    }

    post.getEndorsements().add(endorsedPost);
    posts.add(endorsedPost);
    account.getPosts().add(endorsedPost);
    account.getEndorsedPosts().add(endorsedPost);

    return endorsedPost.getId();
  }

  @Override
  public int commentPost(String handle, int id, String message) throws HandleNotRecognisedException, PostIDNotRecognisedException, NotActionablePostException, InvalidPostException {
    SocialMediaAccount account = Helper.getAccount(handle, accounts);
    SocialMediaPost post = Helper.getPost(id, posts);

    if (account == null) {
      throw new HandleNotRecognisedException();
    }

    if (post == null) {
      throw new PostIDNotRecognisedException();
    }

    if (Boolean.TRUE.equals(post.getEndorsed())) {
      throw new NotActionablePostException();
    }

    SocialMediaPost comment = new SocialMediaPost(account, message);
    posts.add(comment);
    post.getComments().add(comment);
    comment.setParent(post);
    account.getPosts().add(comment);
    return comment.getId();
  }

  @Override
  public void deletePost(int id) throws PostIDNotRecognisedException {
    SocialMediaPost post = Helper.getPost(id, posts);

    if (post == null) {
      throw new PostIDNotRecognisedException();
    }

    post.getEndorsements().clear();
    post.getAccount().getPosts().remove(post);

    String message = "The original content was removed from the system and is no longer available.";
    try {
      post.setMessage(message);
      post.setAccount(null);
    } catch (InvalidPostException e) {
      e.printStackTrace();
    }

    for (SocialMediaPost endorsedPost:post.getEndorsements()) {
      deletePost(endorsedPost.getId());
    }
  }

  @Override
  public String showIndividualPost(int id) throws PostIDNotRecognisedException {
    StringBuilder builder = new StringBuilder();
    SocialMediaPost post = Helper.getPost(id, posts);

    if (post == null) {
      throw new PostIDNotRecognisedException();
    }
      
    builder.append("ID: " + post.getId() + "\n");
    if (post.getAccount() != null) builder.append("Account: " + post.getAccount().getHandle() + "\n");
    builder.append("No. endorsements: " + post.getEndorsements().size() + " | No. comments: " + post.getComments().size() + "\n");
    builder.append(post.getMessage() + "\n");
    
    return builder.toString();
  }

  @Override
  public StringBuilder showPostChildrenDetails(int id) throws PostIDNotRecognisedException, NotActionablePostException {
    StringBuilder builder = new StringBuilder();
    SocialMediaPost post = Helper.getPost(id, posts);

    if (post == null) {
      throw new PostIDNotRecognisedException();
    }

    if (Boolean.TRUE.equals(post.getEndorsed())) {
      throw new NotActionablePostException();
    }

    builder.append(showIndividualPost(post.getId()));

    for (SocialMediaPost currentPost:post.getComments()) {
      String newLine = showPostChildrenDetails(currentPost.getId()).toString();
      String[] lines = newLine.split("\n");
      for (String line:lines) {
        builder.append("     " + line + "\n");
      }
    }
    
    return builder;
  }


  @Override
  public int getNumberOfAccounts() {
    return accounts.size();
  }

  @Override
  public int getTotalOriginalPosts() {
    int counter = 0;
    for (SocialMediaPost post:posts) {
      if (Boolean.FALSE.equals(post.getEndorsed()) && post.getParent() == null && post.getAccount() != null) {
        counter++;
      }
    }
    return counter;
  }

  @Override
  public int getTotalEndorsmentPosts() {
    int counter = 0;
    for (SocialMediaPost post:posts) {
      if (Boolean.TRUE.equals(post.getEndorsed()) && post.getAccount() != null) {
        counter++;
      }
    }
    return counter;
  }

  @Override
  public int getTotalCommentPosts() {
    int counter = 0;
    for (SocialMediaPost post:posts) {
      if (post.getParent() != null && post.getAccount() != null) {
        counter++;
      }
    }
    return counter;
  }

  @Override
  public int getMostEndorsedPost() {
    SocialMediaPost mostEndorsedPost = posts.get(0);
    for (SocialMediaPost post:posts) {
      if (post.getEndorsements().size() > mostEndorsedPost.getEndorsements().size()) {
        mostEndorsedPost = post;
      }
    }

    return mostEndorsedPost.getId();
  }

  @Override
  public int getMostEndorsedAccount() {
    SocialMediaAccount mostEndorsedAccount = accounts.get(0);
    for (SocialMediaAccount account:accounts) {
      if (account.getEndorsedPosts().size() > mostEndorsedAccount.getEndorsedPosts().size()) {
        mostEndorsedAccount = account;
      }
    }

    return mostEndorsedAccount.getId();
  }

  @Override
  public void erasePlatform() {
    accounts.clear();
    posts.clear();
  }

  @Override
  public void savePlatform(String filename) throws IOException {
    FileOutputStream fos = new FileOutputStream(filename);
    ObjectOutputStream oos = new ObjectOutputStream(fos);
    oos.writeObject(this);
    
    oos.close();
    fos.close();
  }

  @Override
  public void loadPlatform(String filename) throws IOException, ClassNotFoundException {
    FileInputStream fis = new FileInputStream(filename);
    ObjectInputStream ois = new ObjectInputStream(fis);
    SocialMedia badSocialMedia = (SocialMedia)ois.readObject();
    this.accounts = badSocialMedia.accounts;
    this.posts = badSocialMedia.posts;

    ois.close();
    fis.close();
  }
}