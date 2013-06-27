\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #19
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 
  d4\p |
  d4. e8 fis4 g |
  a g8 fis e4 b' |
  a4. a8 g4 fis |
  
  e4.( d8) d4 \bar""\break d\cresc |
  d4.\! e8 fis4 g |
  a g8[ fis] e4 b'\pp |
  a4. a8 g4 fis |
  e4.( d8) d4 \break
  
  \repeat volta 2 {
    fis4\p |
    b4. b8 gis4 e |
    a4. a8 fis4 d |
    d'4.\cresc d8\! d4 cis |
    b2 a | \break
    
    b2\rest\p b4. g8 |
    a4. fis8 g4. e8 |
    fis4 b\pp a4. a8 |
    g4 fis e2 |
    d2.
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Since first I saw your face I re -- solv’d
  To hon -- or and re -- nown __ ye;
  If now I be dis -- dain’d,
  I wish my heart had nev -- er known __ ye.
  
  What I that lov’d, and \set associatedVoice = "altos" you that \unset associatedVoice lik’d,
  \set associatedVoice = "tenors"
  Shall we be -- gin to wran -- \set associatedVoice = "altos" gle?
  
  No, no, no, no, no, my heart is fast
  And can -- not dis -- en -- tan -- gle
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  If I ad -- mire or praise you too much,
    That fault you may for -- give __ me.
  Or if my hands had stray’d \set ignoreMelismata = ##t but a touch,
  \unset ignoreMelismata
    Then just -- ly might you leave __ me.	 

  I ask’d you leave, you \set associatedVoice = "altos" bade me \unset associatedVoice love;
  \set associatedVoice = "tenors"
    Is~’t now a time to chide \set associatedVoice = "altos" me?
  No, no, no, no, no, I’ll love you still
    What for -- tune e’er be -- tide me.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The sun whose beams most glo -- ri -- ous are,
  Re -- ject -- eth no be -- hold -- er;
  And your sweet beau -- ty, past com -- pare,
  Made my poor eyes the bold -- er.
  
  Where beau -- ty moves, and \set associatedVoice = "altos" wit de -- \unset associatedVoice lights,
  \set associatedVoice = "tenors"
  And signs of kind -- ness bind \set associatedVoice = "altos" me,
  There, O there, O there! where -- e’er I go,
  I leave my heart be -- hind me.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  d4 |
  d4. d8 d4 d |
  e e8 e e4 e |
  fis4. d8 cis4 d |
  
  d( cis) d d |
  d4. d8 d4 d |
  e e e e |
  fis4. d8 cis4 d |
  d( cis) d
  
  \repeat volta 2 {
    fis4 |
    dis4. dis8 e4 e |
    e4 cis d a' |
    b4. b8 b4 a |
    a( gis) a2 |
    
    e4-. fis-. d4 e |
    cis d b4. cis8 |
    d4 b cis4. cis8 |
    b[ cis] d2 cis4 |
    d2.
  }
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \partial 4
  fis,4 |
  fis4 g a b |
  cis cis8 cis cis4 b8[ cis] |
  d4 a a4 a |
  
  a2 fis4 fis |
  fis4. g8 a4 b |
  cis4 cis cis b8[ cis] |
  
  d4 a a4 a |
  a2 fis4
  
  \repeat volta 2 {
    a4 |
    b4. b8 b4 gis |
    e a a fis |
    fis' g e e |
    e2 cis |
    
    cis4-. d-. b4. cis8 |
    a4. b8 g4 a |
    fis4 g e4. fis8 |
    g[ a] a4 a2 |
    fis2.
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  d,4 |
  d4. d8 d4 b |
  a a8 a a'4 g |
  fis4. fis8 e4 d |
  
  a2 d4 d |
  d4. d8 d4 b |
  a a a' g |
  fis4. fis8 e4 d |
  a2 d4
  
  \repeat volta 2 {
    b4\rest |
    b\rest b e4. e8 |
    cis4 a d4. d8 |
    b4 g gis8 gis a a |
    e'2 a, |
    
    a'4. fis8 g4. e8 |
    fis4. d8 e4. a,8 |
    d4 g, a8[ b] cis[ d] |
    e4 d a2 |
    d2.
  }
}
bassWords = \lyricmode {
  \repeat unfold 31 \skip1
  What I that lov’d, and you that lik’d,
  Shall we be -- gin to wran -- gle?
}
bassWordsII = \lyricmode {
  \repeat unfold 31 \skip1
  I ask’d you leave, you bade me love;
  Is~’t now a time to chide me?
}
bassWordsIII = \lyricmode {
  \repeat unfold 31 \skip1
  Where beau -- ty moves, and wit de -- lights,
  And signs of kind -- ness bind me,
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"
    \new Lyrics = "altosII"
    \new Lyrics = "altosIII"
    \new Lyrics = "altosIV"
    \new Lyrics = "altosV"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \lyricsto "basses" \bassWords
    \new Lyrics \lyricsto "basses" \bassWordsII
    \new Lyrics \lyricsto "basses" \bassWordsIII
    
    \context Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \context Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \context Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Since first I saw your face"}}
  composer = \markup\oldStyleNum"Thomas Ford (c. 1580–1648)"
  tagline = ""
}}




